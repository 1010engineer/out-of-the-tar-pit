;; This is the RF Built-in Evaluator
;; It can evaluate simple standard fns.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLT-isms are marked with "; PLT"

(module rf-builtin-evaluator mzscheme
  (require (lib "list.ss" "srfi" "1")) ; NB - Not 100% pure SRFI-1 - name clashes given "s:" prefix
  (require (lib "23.ss" "srfi")) ; Error SRFI
  (require (lib "time.ss" "srfi" "19")) ; Date / Time SRFI ; with renames (prefix "srfi:")
                                        ; where clashes occur - srfi:make-date, srfi:date? ...
                                        ; beware "date?" will be the MzScheme one....
                                        ; We mostly just use the SRFI 19 "time" structure though

  (require "rf-base.ss")
  (require "rf-catalog.ss")             ; catalog-fn/haskell?
  (require "rf-core.ss")                ; core-add-evaluator
  (require "rf-expr-defs.ss")

  (define (eval-scheme-binary-fn fn arg1 arg2)
      ;; TBA - do arg type checks....
      (if (not (procedure? fn))
          (error "eval-scheme-binary-fn must be called with a proc as its first arg" fn arg1 arg2)
          (fn arg1 arg2)))

  (define (eval-scheme-unary-fn fn arg)
      ;; TBA - do arg type checks....
      (if (not (procedure? fn))
          (error "eval-scheme-unary-fn must be called with a proc as its first arg" fn arg)
          (fn arg)))

  (define (eval-macro-with-exps when mname expr-list . optional-env)
      (let ((expr1 (first expr-list))
            (expr2 (second expr-list)))
;         (display "eval-macro-with-exps:") (newline)
;         (display (expr->string expr1)) (newline)
;         (display (expr->string expr2)) (newline)
;         (display "...") (newline)
        (case mname
          ((and) (and (apply core-eval-expr when expr1 optional-env)
                      (apply core-eval-expr when expr2 optional-env)))
          ((or) (or (apply core-eval-expr when expr1 optional-env)
                    (apply core-eval-expr when expr2 optional-env)))
          (else (error "Builtin evaluator cannot evaluate macro" mname)))))

  (define (eval-indepedent-fn-with-values fname value-list)
      ;; NB - the elements of the value-list are _NOT_ evaluated any further
      ;;(display (string-append "builtin : " (symbol->string fname) " "
      ;;                        (apply string-append (map expr->string value-list)) "\n"))
    (case fname
      ((+ - * / > < >= <= =) (eval-scheme-binary-fn
                              (second (assoc fname (zip '(+ - * / > < >= <= =)
                                                        (list + - * / > < >= <= equal?))))
                              (first value-list)
                              (second value-list)))
      ((sum max min avg all any) (eval-scheme-binary-fn
                                  (second (assoc fname (zip '(sum max min avg all any)
                                                            (list agg-sum agg-max agg-min agg-avg agg-all agg-any))))
                                  (first value-list)
                                  (second value-list)))
      ((add-day add-minute) (eval-scheme-binary-fn
                              (second (assoc fname (zip '(add-day add-minute)
                                                        (list add-day add-minute))))
                              (first value-list)
                              (second value-list)))
      ((not count) (eval-scheme-unary-fn (second (assoc fname (zip '(not count)
                                                                   (list not cardinality))))
                                         (first value-list)))
      (else (error "Builtin evaluator cannot evaluate independent fn" fname))))

  (define (cardinality rel)
    (length (rel/body rel)))

  (define (agg-any rel attr-name) (agg-operator-eval rel attr-name (lambda (a b) (and a b)) #f)) ; TBA - is this the efficient way round?
  (define (agg-all rel attr-name) (agg-operator-eval rel attr-name (lambda (a b) (or a b)) #t)) ; ditto...?
  (define (agg-sum rel attr-name) (agg-operator-eval rel attr-name + 0))
  (define (agg-max rel attr-name) (agg-operator-eval rel attr-name (lambda (a b) (if (> a b) a b)) 0))
  (define (agg-min rel attr-name) (agg-operator-eval rel attr-name (lambda (a b) (if (< a b) a b)) 0))
  (define (agg-avg rel attr-name) (if (= 0 (cardinality rel)) 0 (/ (agg-sum rel attr-name) (cardinality rel))))

  (define (agg-operator-eval rel attr-name op ridentity) ; NB ridentity is only used if rel is empty
      ;; This will be typically (indirectly) invoked by something like:
      ;; "(max product 'coupon)" - NB the var-ref needs to be quoted so that it doesn't get treated
      ;; like a relation-variable.....
      ;; TBA - We need to add in a generic "aggregate" operator that will fold / reduce a
      ;; Haskell fn through the set of values for an attribute in a relation!!!
      (let ((attr-index (heading/attr-index (rel/heading rel) (var-ref/name attr-name))))
        (if (eq? attr-index #f)
            (error "Attribute not found in relation whilst trying to perform aggregate operation" attr-name op)
            (reduce op ridentity (map (lambda (l) (list-ref l attr-index)) (rel/body rel))))))

  (define (add-day when value)
    (make-time TIME-UTC
               (time-nanosecond when)
               (+ (time-second when) (* (* 60 60 24) value))))

  (define (add-minute when value)
    (make-time TIME-UTC
               (time-nanosecond when)
               (+ (time-second when) (* (* 60 60) value))))

  ;; Register with the core when the module is loaded...
  (core-register core-register-independent-fn eval-indepedent-fn-with-values '(count not + - * / > < >= <= =))
  (core-register core-register-independent-fn eval-indepedent-fn-with-values '(sum max min avg all any))
  (core-register core-register-independent-fn eval-indepedent-fn-with-values '(add-day add-minute))
  (core-register core-register-macro eval-macro-with-exps '(and or))
)
