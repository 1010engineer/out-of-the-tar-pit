;; This is the RF Core
;; ...it coordinates between all the component modules...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLT-isms are marked with "; PLT"

(module rf-core mzscheme
  (require (lib "list.ss" "srfi" "1")) ; NB - Not 100% pure SRFI-1 - name clashes given "s:" prefix
  (require (lib "time.ss" "srfi" "19")) ; Date / Time SRFI ; with renames (prefix "srfi:")
                                        ; where clashes occur - srfi:make-date, srfi:date? ...
                                        ; beware "date?" will be the MzScheme one....
                                        ; We mostly just use the SRFI 19 "time" structure though
	(require "rf-base.ss")
	(require "rf-expr-defs.ss")

  (define *runloop-entries* '())        ; alist of (waitable -> callback-proc)
  (define *context-fns* '())            ; alist of (fn -> eval-proc)
  (define *independent-fns* '())        ; alist of (fn -> eval-proc)
  (define *macros* '())                 ; alist of (fn -> eval-proc)
  (define *shutdown-hooks* '())
  (define *stored-var-resolvers* '())

  (define *registered-default-independent-evaluator* '()) ; This will typically be the Haskell fn evaluator

  (define *monitored-expressions* '())

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Below this is the publicly-exported API
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (runloop)
      (let* ((waitable (apply object-wait-multiple #f (map car *runloop-entries*))) ; PLT v209
                                        ; PLT v300 has renamed object-wait-multiple to something else...
             (callback (cdr (assoc waitable *runloop-entries*))))
        ;; NB - This code assumes that all the waitables return themselves - ok
        ;; for ports and socket-listeners....
        (callback))
    (runloop))

  ;; add a waitable (which should return itself when triggered) to the runloop
  (define (core-add-to-runloop waitable callback)
      (set! *runloop-entries* (alist-cons waitable callback *runloop-entries*)))

  (define (core-remove-from-runloop waitable)
      (set! *runloop-entries* (alist-delete waitable *runloop-entries* equal?)))

  ;; add an expression to the monitored list
  (define (core-monitor-expr expr context callback)
      (if (not (expr/has-free-vars? null expr)) ; TBA - is this 'null' correct??
          (error "There's no point in monitoring a grounded expr! Its value will never change..." expr)
          (set! *monitored-expressions* (alist-cons `(,expr ,context) callback *monitored-expressions*))))

  (define (core-stop-monitoring context)
      ;; Remove all monitorings with the same context
      (set! *monitored-expressions* (alist-delete `(null ,context) *monitored-expressions*
                                                  (lambda (key1 key2)
                                                    (equal? (second key1) (second key2))))))

  (define (update-monitored-expressions)
      (display "update-monitored-expressions\n")
      (map (lambda (mon-entry)
             (display mon-entry) (newline)
             (let* ((callback (cdr mon-entry))
                    (expr (caar mon-entry))
                    (context (cdar mon-entry))
                    (whentime (current-time TIME-UTC)) ; TBA - probably ought to get from an arg...
                                        ; ...but we do need to ensure that whatever time is used
                                        ; does reflect the _results_ of this operation - ie it will
                                        ; need to be _after_ the 'when' that was used to evaluate
                                        ; the new value for the assign - actually, more precisely it
                                        ; will need to be _after_ whatever timestamp is put on the
                                        ; relvar that was updated by the assignment that triggered
                                        ; this update...
                     ; See the notes with 'asof' in expr-defs
                     ; for why the workaround below is necessary for the PLT v209 impl of SRFI 19
                    (one-sec-from-now (make-time TIME-UTC
                                                 (time-nanosecond whentime)
                                                 (+ (time-second whentime) 1)))
                    (new-value (core-eval-expr one-sec-from-now expr)))
               (callback new-value)))
           *monitored-expressions*))

  (define (core-register register-fn evaluator-proc fn-list)
      (map (lambda (f) (register-fn evaluator-proc f)) fn-list))

  (define (core-register-context-fn evaluator-proc fn)
      (set! *context-fns* (alist-cons fn evaluator-proc *context-fns*)))
  (define (core-register-independent-fn evaluator-proc fn)
      (set! *independent-fns* (alist-cons fn evaluator-proc *independent-fns*)))
  (define (core-register-macro evaluator-proc fn)
      (set! *macros* (alist-cons fn evaluator-proc *macros*)))

  (define (core-register-default-independent-evaluator evaluator-proc)
      (set! *registered-default-independent-evaluator* evaluator-proc))

  (define (core-register-shutdown-hook hook)
      (set! *shutdown-hooks* (cons hook *shutdown-hooks*)))

  (define (core-register-stored-var-resolver resolver)
      (set! *stored-var-resolvers* (cons resolver *stored-var-resolvers*)))

  ;; TBA 2 - "variable-name" needs to be able to cope with general "expression"s to
  ;; take accout of any 'master' JOINs or EXTENSIONs.... that the client - eg RFExpression -
  ;; might be using
  (define (core-assign when pre-condition-expr variable-name expr)
      ;; This method subsumes separate INSERT / UPDATE / DELETE methods
      ;; (they can all be simply implemented in terms of this one)
      ;; it will check a pre-condition (ensure that it evaluates to #t)
      ;; and then evaluate expr, and assign it to 'variable-name',
      ;; check that no constraints are violated and return
      ;; TBA
      (display (string-append "core-assign: if "
                              (expr->string pre-condition-expr) " then set "
                              (symbol->string variable-name) " to "
                              (expr->string expr)))
    (display "TBA - WARNING - multiple assignments in same second are not guaranteed!") ; see rf-expr-defs asof notes
    (if (not (core-eval-expr when pre-condition-expr))
        (error "assignment failed because the precondition was not satisfied"
               (expr->string pre-condition-expr) variable-name expr)
        (let ((new-value (core-eval-expr when expr)))
          (store-var-ref variable-name new-value *stored-var-resolvers*)
          (display "TBA - Check that constraints are satisfied here and rollback if needed\n")
          (update-monitored-expressions))))

  ;; core-eval-expr-------------------------------------------------------
  (define (core-eval-expr when expr . optional-env)
      ;; NB - the "when" here is _not_ an expr, it is a _value_
      ;; NB - "when"s are represented by SRFI 19 "time" structures with UTC time base
      ;;
;;          (display "core-eval-expr: when:")
;;          (display (expr->string when))
;;          (display " expr:")
;;          (display (expr->string expr))
;;          (newline)
      (cond
        ((and (null? when) (apply expr/has-free-vars? when expr optional-env))
         ;; The assumption is that if the expression contains free variables
         ;; they must be references to _STORED_ variables - and hence they
         ;; can only be evaluated in the context of a "when"
         (error "It is not possible to evaluate a non-grounded expr without a timebase!" when expr))
        ((not (or (null? when) (time? when)))
         (error "core-eval-expr must be called with an _evaluated_ date - it does _not_ eval its 'when' arg!"
                when
                expr))
        ((asof? expr) (apply core-eval-expr
                             (apply core-eval-expr when (asof/when-expr expr) optional-env)
                             (asof/expr expr)
                             optional-env))
        ((literal? expr) expr)            ; :literals by defn evaluate to themselves
        ((quoted-expr? expr) (quoted-expr/expr expr))
        ((func-invoc-repr? expr) (apply eval-func-invoc-repr when expr optional-env))
        ((var-ref? expr)
         (let ((vname (var-ref/name expr)))
           (cond ((eq? vname 'now) when)
                 ((assoc vname optional-env) (second (assoc vname optional-env)))
                 (else (resolve-stored-var-ref when vname *stored-var-resolvers*)))))
        (else (error "Unknown <expr> type cannot be evaluated:" expr))))

  (define (eval-func-invoc-repr when fir . optional-env)
      ;;(display (string-append "EVAL: " (expr->string fir) " " (stringify optional-env) "\n"))
    (let ((fname (func-invoc-repr/name fir))
          (arg-exprs (func-invoc-repr/arg-exprs fir)))
          ;; NB - an _INDEPENDENT_ fn is one whose result is not dependent upon
          ;; either an environment or a 'when'. eg Equality Fns _MUST_ be of this
          ;; nature. (One way to think about this is to consider that the only
          ;; reason for requiring either an environment or a 'when' is because
          ;; different results may be returned depending upon the when/env
          ;; supplied... this obviously must not happen for equality fns...
          ;; eg the value "(+ a b)" is _always_ equal to the value "(+ a b)"
          ;; ...the fact that the values are expressions which could themselves
          ;; be evaluated in different envs is irrelevant - because they are
          ;; _NOT_ being evaluated here...)
          ;; So, "custom-equal-fn" when applied to the list of values '((+ a b) (+ a b))
          ;; will always return #t. (because the values don't get 'evaluated' themselves)
          ;; This is in contrast with - say - "restrict" applied to the list
          ;; of values: '((rel ((col1 col2) (int int) (3 9))) (> col1 constant-from-environment))
          ;;    [this may come from say:
          ;;        "(restrict MYTABLE (quote (> col1 constant-from-environment)))"]
          ;; The crucial difference is whether the evaluation of the fn will
          ;; itself require futher evaluation of the argument _values_.
          ;;
          ;; OK.......... the bottom line is this:
          ;; there are _TWO_ types of function:
          ;; -------------------------------------------------------------------------------------|
          ;; 1) "Independent" functions (such as those implemented in Haskell...)                 |
          ;; 2) "Context" functions (those which depend upon the evaluation mechanism of the core)|
          ;; -------------------------------------------------------------------------------------|
          ;; Equality fns must be INDEPENDENT FNS. _Some_ relational operators are CONTEXT FNS.
          ;; To evaluate an Independent fn, you just supply it with its arg _values_.
          ;; To evaluate a Context fn you must also supply:
          ;;    A) An Environment
          ;;    B) A "When"
          ;;    C) A way for it to call back into the core-eval routine
          ;; The general expectation is that by far the _majority_ of fns in the system will
          ;; be INDEPENDENT FNs. The only CONTEXT FNs will be special in-built ones such
          ;; as "restrict", "extend", "and", "or". NB "union" and "project" are Independent...
          ;;
          ;; NB - Because the general assumption is that most fns will be _INDEPENDENT_, the
          ;; term "function" when unqualified should be taken to mean _independent_ fn.
          ;;
          ;; NB2 - CONTEXT FNs are futher divided into normal context fns and "Macros" which
          ;; take on responsability for the evaluation of their own arguments (eg "or" "if" "and")
          (if (fn-macro? fname)
              (apply eval-context-fn-with-values when fname *macros* arg-exprs optional-env)
              (let ((arg-values (map (lambda (x) (apply core-eval-expr when x optional-env)) arg-exprs)))
                (if (fn-context? fname)
                    (apply eval-context-fn-with-values when fname *context-fns* arg-values optional-env)
                    (core-eval-indepedent-fn-with-values fname arg-values))))))

  (define (fn-macro? fname) (assoc fname *macros*))
  (define (fn-context? fname) (assoc fname *context-fns*))

  (define (resolve-stored-var-ref when vname resolvers)
      (or ((car (first resolvers)) when vname)
          (resolve-stored-var-ref when vname (cdr resolvers))))

  (define (store-var-ref vname value resolvers)
      (or ((cdr (first resolvers)) vname value)
          (store-var-ref vname value (cdr resolvers))))

  (define (eval-context-fn-with-values when fname evaluators value-list . optional-env)
      ;; NB - the elements of the value-list are _NOT_ evaluated any further
      ;; This is a CONTEXT FUNCTION - ie one that is dependent upon
      ;; of a "when" and an environment
      ;; NB2 - this fn is _not_ exported outside the module - people calling
      ;; back in must use "core-eval-expr" instead....
;;     (display "eval-context-fn-with-values : ")
;;     (display fname)
;;     (display (map (lambda (x) (string-append " " (expr->string x))) value-list))
;;     (newline)
    (apply (cdr (assoc fname evaluators)) when fname value-list optional-env))

  (define (core-eval-indepedent-fn-with-values fname value-list)
      ;; NB - the elements of the value-list are _NOT_ evaluated any further
      (let* ((registered-evaluator (if (assoc fname *independent-fns*)
                                       (cdr (assoc fname *independent-fns*))))
             (proc (if (void? registered-evaluator)
                       *registered-default-independent-evaluator*
                       registered-evaluator)))
;;          (display (string-append "core-eval-indepedent-fn-with-values "
;;                                  (symbol->string fname) " "
;;                                  (stringify (map stringify value-list)) " "
;;                                  (stringify proc) "\n"))
        (proc fname value-list)))

	(provide runloop
           core-add-to-runloop
           core-remove-from-runloop
           core-monitor-expr
           core-stop-monitoring
           core-eval-indepedent-fn-with-values
           core-register
           core-register-context-fn
           core-register-independent-fn
           core-register-macro
           core-register-shutdown-hook
           core-register-stored-var-resolver
           core-register-default-independent-evaluator
           core-assign
           core-eval-expr)
)
