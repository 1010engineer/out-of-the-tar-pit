;; This is the RF Expr Defs
;; It defines all the basic types used for representing expressions
;; NB - These are _not_ the types as users will see them ... but,
;; _all_ user types are ultimately represented by one of the below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLT-isms are marked with "; PLT"

(module rf-expr-defs mzscheme
  (require (lib "etc.ss" "mzlib"))      ; PLT - "identity"
  (require (lib "9.ss" "srfi"))         ; Record Types SRFI
  (require (lib "list.ss" "srfi" "1"))  ; NB - Not 100% pure SRFI-1 - name clashes given "s:" prefix
  (require (lib "time.ss" "srfi" "19")) ; Date / Time SRFI ; with renames (prefix "srfi:")
                                        ; where clashes occur - srfi:make-date, srfi:date? ...
                                        ; beware "date?" will be the MzScheme one....
                                        ; We mostly just use the SRFI 19 "time" structure though
  (require (rename (lib "list.ss" "mzlib") quicksort quicksort)) ; PLT
  (require "rf-base.ss")                ; stringify...


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; First the 'pseudo' (union) 'types' - ie those not directly represented by Scheme records
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; :expr
  (define (expr? obj) (or (literal? obj)
                          (quoted-expr? obj)
                          (asof? obj)
                          (func-invoc-repr? obj)
                          (var-ref? obj)))
  (define (expr/has-free-vars? when expr . optional-env)
      (cond ((rf-atom? expr) #f)
            ((rel? expr) #f)
            ((quoted-expr? expr) #f)
            ((asof? expr) (or (apply expr/has-free-vars? when (asof/expr expr) optional-env)
                              (apply expr/has-free-vars? #t (asof/when-expr expr) optional-env)))
            ((func-invoc-repr? expr) (any (lambda (e) (apply expr/has-free-vars? when e optional-env))
                                          (func-invoc-repr/arg-exprs expr)))
            ((var-ref? expr) (if (eq? 'now (var-ref/name expr))
                                 (null? when)
                                 (not (assoc (var-ref/name expr) optional-env))))
            (else (error "unknown expr type whilst trying to check free for var refs" expr))))

  (define (expr/=? exp1 exp2)
      ;; Compare two exprs - (without evaluating them!!!)
      ;;
      ;; If the _representations_ of the values are the same, then
      ;; the values are _definitely_ the same
      (or
       (equal? exp1 exp2)
       (and (number? exp1) (number? exp2) (= exp1 exp2)) ; TBA - should probably ditch this
                                        ; and instead rely on all numbers being supplied as 'exact' where
                                        ; they are to be used in rel.operations. (nb (equal? 2.0 2) is #f but
                                        ; (equal? #e2.0 2) is #t as is (= 2.0 2) usually!!!)
       (and (time? exp1) (time? exp2) (time=? exp1 exp2))
       ;;(and (rf-atom? exp1) (rf-atom? exp2) (equal? exp1 exp2))
       (and (rel? exp1) (rel? exp2) (equal? exp1 exp2)) ; TBA - won't work with nested structs etc... FIXME
       (and (var-ref? exp1) (var-ref? exp2) (equal? (var-ref/name exp1) (var-ref/name exp2)))
       (and (quoted-expr? exp1) (quoted-expr? exp2) (expr/=? (quoted-expr/expr exp1) (quoted-expr/expr exp2)))
       (and (asof? exp1)
            (asof? exp2)
            (expr/=? (asof/when-expr exp1) (asof/when-expr exp2))
            (expr/=? (asof/expr exp1) (asof/expr exp2)))
       (and (func-invoc-repr? exp1)
            (func-invoc-repr? exp2)
            (equal? (func-invoc-repr/name exp1) (func-invoc-repr/name exp2))
            (every identity (map (lambda (a b) (expr/=? a b)) ; PLT - identity
                                 (func-invoc-repr/arg-exprs exp1)
                                 (func-invoc-repr/arg-exprs exp2))))))
  (provide expr? expr/has-free-vars? expr/=?)

  (define (expr->string expr)
      (cond ((time? expr) (date->string (time-utc->date expr)))
            ((rf-atom? expr) (stringify expr))
            ((rel? expr) (string-append "{" (stringify (rel->sexp expr)) "}"))
            ((quoted-expr? expr) (string-append "\"" (expr->string (quoted-expr/expr expr)) "\""))
            ((asof? expr) (string-append "\""
                                         (expr->string (asof/expr expr))
                                         (expr->string (asof/when-expr expr))
                                         "\""))
            ((func-invoc-repr? expr) (string-append (symbol->string (func-invoc-repr/name expr))
                                                    "[ " (apply string-append
                                                               (map (lambda (e)
                                                                      (string-append (expr->string e) " "))
                                                                    (func-invoc-repr/arg-exprs expr)))
                                                    "]"))
            ((var-ref? expr) (string-append "<" (symbol->string (var-ref/name expr)) ">"))
            (else (string-append "???" (stringify expr) "???"))))
  (provide expr->string)

  ;; :literal
  (define (literal? obj) (or (rf-atom? obj)
                             (rel? obj)))
  (provide literal?)

  ;; :rf-atom - All of these are represented by their Scheme counterparts
  (define (rf-atom? obj) (or (string? obj)
                             (symbol? obj)
                             (integer? obj)
                             (real? obj)
                             (boolean? obj)
                             (time? obj)))
  (define (make-atom obj)          ; At the moment this is a no-op, but provides encapsulation of atom type
      (if (not (rf-atom? obj)) (error "Can't construct atom from non atom value" obj) obj))
  (provide rf-atom? make-atom)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Now the 'real' types (ie structs etc...)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; :quoted-expr
  (define-record-type :quoted-expr
      (make-quoted-expr expr)
    quoted-expr?
    (expr quoted-expr/expr))
  (provide make-quoted-expr quoted-expr? quoted-expr/expr)

  ;; :asof - wraps an expr indicating it should be eval'd "As Of" a
  ;; certain time in the past (represented by an expr that evaluates to a "when")
  ;; NB - "when"s are represented by SRFI 19 "time" structures with UTC time base
  ;; WARNING - The current PLT v209 implementation is broken at the sub-second level
  ;; as demonstrated by the following test fn:
  ;; (define (timetest n lasttime)
  ;;     (if (> n 0)
  ;;         (let ((t (current-time TIME-MONOTONIC)))
  ;;            (display (time-second t))
  ;;            (display ":")
  ;;            (display (time-nanosecond t))
  ;;            (display " - ")
  ;;            (if (not (null? lasttime))
  ;;                (display (time>? t lasttime))) ; Will sometimes display #f !!!!
  ;;            (newline)
  ;;            (sleep 0.2)
  ;;            (timetest (- n 1) t))
  ;;         (display 'done)))
  ;;
  ;;  It turns out that this is because internally it uses:
  ;;    (define (tm:get-time-of-day)
  ;;       (values (current-seconds)
	;;               (abs (remainder (current-milliseconds) 1000))))
  ;;  to return the seconds / nanoseconds .... and current-milliseconds is not defined
  ;;  to have any relationship to current-seconds at all....
  (define-record-type :asof
      (make-asof when-expr main-expr)
    asof?
    (main-expr asof/expr)               ; NB - note that the <asof>'s 'when' is itself an <expr>. (This is vital
                                        ; to enable us to express expressions such as "the increase in 'price'
                                        ; over the last day") - eg:
                                        ; (subtract (asof now price) (asof (date-subtract-days now 1) price))
                                        ; [ nb - "now" is a slightly special :var-ref - see below ]
                                        ;
                                        ; "now" is mostly treated like a normal :var-ref - only difference
                                        ; is that its binding is handled specially by the core eval loop
                                        ; which will bind it to its 'when' as appropriate.
                                        ;
                                        ; evaluation of an expr which has free-variables (ie variables that
                                        ; presumably refer to _STORED_ values) can only occur in the context
                                        ; of a "when" - which is supplied to the core-eval fn
    (when-expr asof/when-expr))
  (provide make-asof asof? asof/expr asof/when-expr)

  ;; :func-invoc-repr
  (define-record-type :func-invoc-repr
      (make-func-invoc-repr name arg-exprs)
    func-invoc-repr?
    (name func-invoc-repr/name)         ; This should be a symbol representing the fn name
    (arg-exprs func-invoc-repr/arg-exprs))
  (provide make-func-invoc-repr func-invoc-repr? func-invoc-repr/name func-invoc-repr/arg-exprs)

  ;; :var-ref
  (define-record-type :var-ref
      (make-var-ref var-name)
    var-ref?
    (var-name var-ref/name))            ; The name should be represented as a symbol
  (provide make-var-ref var-ref? var-ref/name)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Relation / Tuple / Heading / Attribute types
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; :attr
  (define-record-type :attr
      (build-attr name type)
    attr?
    (name attr/name)                    ; Represented as a symbol
    (type attr/type))                   ; Represented as a symbol
  (define (attr/eq? att1 att2) (and (eq? (attr/name att1) (attr/name att2))
                                    (eq? (attr/type att1) (attr/type att2))))
  (define (attr->string att) (string-append "{" (symbol->string (attr/name att)) ":"
                                              (symbol->string (attr/type att)) "}"))
  (define (make-attr name type)
      (if (not (and (symbol? name) (symbol? type)))
          (error "make-attr must be called with two symbols" name type)
          (build-attr name type)))
  (provide make-attr attr/name attr/type attr->string)

  ;; :heading
  (define-record-type :heading
      (build-heading attrs)
    heading?
    (attrs heading/attrs))
  (define (make-heading attrs)
      (if (not (attrs-in-order? attrs))
          (error "attrs must be in name-ascending order to construct a heading" (map attr->string attrs))
          (build-heading attrs)))
  (define (heading-eq? heading1 heading2)
      ;; If we were prepared to assume that the heading attr lists were always
      ;; in att-name string ascending order (as they always should be) then we
      ;; could use a simple list equality check here... play safe for now though...
      (lset= attr/eq? (heading/attrs heading1) (heading/attrs heading2)))
  (define (attrs-in-order? attrs)
      ;; Check that all the attr names are in ascending alphabetical order...
      (or (= 1 (length attrs))
          (apply string<? (map (lambda (attr) (symbol->string (attr/name attr))) attrs))))
  (define (heading/type-for-name heading attr-name)
      (any (lambda (attr) (if (eq? attr-name (attr/name attr)) (attr/type attr) #f))
           (heading/attrs heading)))
  (define (heading/attr-index heading attr-name)
      (list-index (lambda (attr) (eq? attr-name (attr/name attr))) (heading/attrs heading)))
  (define (heading->string heading)
      (string-append "(**" (stringify (map attr->string (heading/attrs heading))) "**)"))
  (define (heading/valid? heading)
      ;; TBA - check that all the types exist too...
      ;; TBA - check that there are no duplicates names...
      (attrs-in-order? (heading/attrs heading)))
  (provide make-heading heading-eq? heading/attrs heading/valid? heading/type-for-name heading->string heading/attr-index)

  ;; :tuple
  (define-record-type :tuple
      (make-tuple heading body)
    tuple?
    (heading tuple/heading)               ; This will be a :heading
    (body tuple/body))                    ; This will be a list of :exprs
  (define (tuple/attrs tuple) (heading/attrs (tuple/heading tuple)))
  (define (tuple->string tuple) (string-append "<<" (stringify (map expr->string (tuple/body tuple))) ">>"))
  (provide make-tuple tuple? tuple/heading tuple/body tuple/attrs tuple->string)

  ;; :rel
  (define-record-type :rel
      (build-rel heading body)
    rel?
    (heading rel/heading)               ; This will be a :heading
    (body rel/body))                    ; This will be a list of lists of :exprs
  (define (make-rel heading body-exprs) ; body-exprs should be a list of :exprs
      ;;(display (string-append "make-rel:" (heading->string heading) "\n"))
      ;;(display body-exprs) (newline)
      (if (not (= (* (length (heading/attrs heading)) (length body-exprs)) ; #values expected in whole relation
                  (fold (lambda (row so-far) (+ (length row) so-far)) 0 body-exprs))) ; actual #values
          (error "Attempt to make relation from non-consistent components" (heading->string heading) body-exprs)
          (build-rel heading body-exprs)))
  (define (rel/attrs rel) (heading/attrs (rel/heading rel)))
  (define (sexp->rel l)
      (if (not (and (pair? l) (pair? (first l)) (pair? (second l)) (pair? (third l))))
          (error "non-list found when trying to build relation from list" l)
          (make-rel (make-heading (map make-attr (first l) (second l)))
                    (map (lambda (row)
                           (map (lambda (val) (sexp->expr val identity)) row)) ; PLT - identity
                         (cddr l)))))
  (define (rel->sexp rel)
      (if (not (rel? rel))
          (error "non-relation found when trying to build list from relation" rel)
          (let ((attrs (heading/attrs (rel/heading rel))))
            `(rel (,(map attr/name attrs) ,(map attr/type attrs) ,@(map (lambda (row)
                                                                          (map expr->sexp row))
                                                                        (rel/body rel)))))))
  (provide make-rel rel/attrs rel? rel/heading rel/body sexp->rel rel->sexp)

  (define (expr->sexp expr)            ; Remove all the custom :structs
      (cond ((time? expr) (when->sexp expr))
            ((rf-atom? expr) expr)
            ((rel? expr) (rel->sexp expr))
            ((quoted-expr? expr) `(quote ,(expr->sexp (quoted-expr/expr expr))))
            ((asof? expr) `(asof ,(expr->sexp (asof/expr expr))
                                 ,(expr->sexp (asof/when-expr expr))))
            ((func-invoc-repr? expr) `(,(func-invoc-repr/name expr)
                                        ,@(map expr->sexp (func-invoc-repr/arg-exprs expr))))
            ((var-ref? expr) (var-ref/name expr))
            (else (string-append "?" (stringify expr) "?"))))
  (provide expr->sexp)

  (define (sexp->when encoded-date encoded-time)
      ;; encoded-date will be eg '26/8/2002
      ;; encoded-time will be eg '17:00:00
      ;; NB - a "when" is actually a SRFI-19 "time-utc"
      (date->time-utc
       (string->date (string-append (symbol->string encoded-date) " " (symbol->string encoded-time))
                    "~d/~m/~Y ~H:~M:~S")))

  (define (when->sexp when)
      (let ((date (time-utc->date when)))
        (list 'when
              (string->symbol (date->string date "~d/~m/~Y"))
              (string->symbol (date->string date "~H:~M:~S")))))

  (define (sexp->expr sexp symbol-handler)
      ;; Convert a raw sexp (eg from (read)) into an <expr>
      (cond ((pair? sexp) (if (symbol? (car sexp))
                             (case (car sexp)
                               ((rel) (sexp->rel (second sexp)))
                               ((when) (sexp->when (second sexp) (third sexp)))
                               ((quote) ; inside a quote, all symbols are :var-refs...
                                (make-quoted-expr (sexp->expr (second sexp) make-var-ref)))
                               ((asof)
                                (make-asof (sexp->expr (second sexp) make-var-ref)
                                           (sexp->expr (third sexp) make-var-ref)))
                               (else (make-func-invoc-repr (car sexp) ; should be a symbol
                                                           (map (lambda (x) (sexp->expr x make-var-ref))
                                                                (cdr sexp)))))
                             (error "badly formatted expression - func invocs must start with a symbol" sexp)))
            ((symbol? sexp) (symbol-handler sexp))
            ((string? sexp) (string->symbol sexp))
            ((or (integer? sexp) ; These are all the literals
                 (real? sexp)
                 (boolean? sexp)) (make-atom sexp))
            (else (error "invalid-expression" sexp))))

  (define (format-list-for-relation l)  ; Sorts the columns so that they are in attr-name order
      (let* ((names (first l))
             (types (second l))
             (rows (cddr l)))
        (define (sort-by-names values)
            (map second (quicksort (zip names values) ; PLT
                                       (lambda (nv1 nv2) (string<? (symbol->string (car nv1))
                                                                   (symbol->string (car nv2)))))))
        (append (list (sort-by-names names))
                (list (sort-by-names types))
                (map sort-by-names rows))))
  (provide sexp->expr format-list-for-relation)
)
