;; This is the RF Relational Evaluator
;; It can evaluate relational expressions.
;; More precisely it can evaluate the 'operators'
;; of the relational algebra - join, union etc...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLT-isms are marked with "; PLT"

(module rf-rel-evaluator mzscheme
  (require (lib "etc.ss" "mzlib"))      ; PLT - "identity"
  (require (lib "list.ss" "srfi" "1")) ; NB - Not 100% pure SRFI-1 - name clashes given "s:" prefix
  (require (rename (lib "list.ss" "mzlib") mergesort mergesort)) ; PLT
  (require (lib "23.ss" "srfi")) ; Error SRFI

  (require "rf-base.ss")
  (require "rf-catalog.ss")             ; catalog-fn/haskell?
  (require "rf-core.ss")                ; core-add-evaluator
  (require "rf-expr-defs.ss")

  (define (heading-merge heading1 heading2 f)
      (let* ((attrs1 (heading/attrs heading1))
             (attrs2 (heading/attrs heading2))
             (name-type1 (zip (map attr/name attrs1) (map attr/type attrs1)))
             (name-type2 (zip (map attr/name attrs2) (map attr/type attrs2)))
             (common-name-types (f equal? name-type1 name-type2)) ; do the union / intersection
             (sorted-name-types (mergesort common-name-types ; mergesort because expected to be nearly in-order
                                           (lambda (nt1 nt2) (string<? (symbol->string (car nt1))
                                                                       (symbol->string (car nt2)))))))
        (make-heading (map make-attr (map first sorted-name-types) (map second sorted-name-types)))))

  (define (common-heading heading1 heading2)
      (heading-merge heading1 heading2 lset-intersection))

  (define (union-heading heading1 heading2)
      (heading-merge heading1 heading2 lset-union))

  (define (test-common-heading)
      (let ((h1 (make-heading (map make-attr '(|Age| |Colour| |Name|) '(integer string string))))
            (h2 (make-heading (map make-attr
                                   '(|Age| |Cap| |Colour| |Size| |Weight|)
                                   '(integer real string real integer)))))
        (display (heading->string (common-heading h1 h2)))))

  (define (tuple-project tuple projection-heading)
      (let ((names-to-keep (map attr/name (heading/attrs projection-heading))))
        (make-tuple projection-heading
                    (map second
                         (filter (lambda (name-value) (member (car name-value) names-to-keep))
                                 (zip (map attr/name (heading/attrs (tuple/heading tuple)))
                                      (tuple/body tuple)))))))

  (define (test-tuple-project)
      (let* ((hdg (make-heading (map make-attr
                                    '(Age Cap Colour Size Weight)
                                    '(integer string string real integer))))
             (proj-hdg (make-heading (map make-attr '(Cap Size) '(string real))))
             (tup (make-tuple hdg '(38 "FlatCap" "Red" 89.8 23))))
        (display (tuple->string (tuple-project tup proj-hdg)))))

  (define (tuples-match? tuple1 tuple2 common-hdg)
      (tuple-eq? (tuple-project tuple1 common-hdg) (tuple-project tuple2 common-hdg)))

  ;; TBA - what is corrrect behaviour if the two headings have a common
  ;; attr-name but with different types? for now we will just consider
  ;; that as a non-matching attr ... (think that's the only sensible
  ;; approach....)
  (define (join-tuples-using-heading tuple1 tuple2 common-hdg)
      (if (tuples-match? tuple1 tuple2 common-hdg)
          (tuple-union tuple1 tuple2)
          #f))

  (define (tuple-union tuple1 tuple2)
      ;; This proc _assumes_ that the tuples are compatible - clients must check this first
      (let* ((attrs1 (tuple/attrs tuple1))
             (attrs2 (tuple/attrs tuple2))
             (name-type-body1 (zip (map attr/name attrs1) (map attr/type attrs1) (tuple/body tuple1)))
             (name-type-body2 (zip (map attr/name attrs2) (map attr/type attrs2) (tuple/body tuple2)))
             (common-ntbs (lset-union equal? name-type-body1 name-type-body2))
             (sorted-ntbs (mergesort common-ntbs ; mergesort because expected to be nearly in-order
                                     (lambda (ntb1 ntb2) (string<? (symbol->string (car ntb1))
                                                                   (symbol->string (car ntb2)))))))
        (display "WARNING lset-union is _ASYMMETRIC_!!!! need to re-check _ALL_ codebase to ensure it's being used correctly\n")
        (make-tuple
         (make-heading (map make-attr (map first sorted-ntbs) (map second sorted-ntbs)))
         (map third sorted-ntbs))))

  (define (cross-product-tuples heading1 heading2 body1 body2)
      (map (lambda (body-pair) (cons (make-tuple heading1 (car body-pair))
                                     (make-tuple heading2 (cdr body-pair))))
                   (cross-product body1 body2)))

  (define (cross-product list1 list2)
      (cond ((or (null? list1) (null? list2)) '())
            (else
             (append (map (lambda (l1-val) (cons l1-val (car list2))) list1)
                  (cross-product list1 (cdr list2))))))

  (define (join-tuples tuple1 tuple2)
      (join-tuples-using-heading tuple1 tuple2 (common-heading (tuple/heading tuple1) (tuple/heading tuple2))))

  (define (test-join-tuples)
      (let* ((t1 (make-tuple (make-heading (map make-attr '(Age Cap Colour) '(integer string string)))
                             '(38 "FlatCap" "Red")))
             (t2 (make-tuple (make-heading (map make-attr '(Cap Colour Size Weight)
                                                '(string string real integer)))
                             '("FlatCap" "Red" 89.8 23))))
        (display (heading->string (common-heading (tuple/heading t1) (tuple/heading t2))))
        (display (tuple->string (join-tuples t1 t2)))))

  (define (join rel1 rel2)
      ;; Probably hideously inefficient....
      (let ((common-hdg (common-heading (rel/heading rel1) (rel/heading rel2))))
        (make-rel (union-heading (rel/heading rel1) (rel/heading rel2))
                  (map (lambda (tuple-pair) (tuple/body (tuple-union (car tuple-pair) (cdr tuple-pair))))
                       (filter (lambda (tuple-pair)
                                 (tuples-match? (car tuple-pair) (cdr tuple-pair) common-hdg))
                               (cross-product-tuples (rel/heading rel1)
                                                     (rel/heading rel2)
                                                     (rel/body rel1)
                                                     (rel/body rel2)))))))

  (define (test-join)
      (let* ((r1 (make-rel (make-heading (map make-attr '(Age Cap Colour) '(integer string string)))
                             '((38 "FlatCap" "Red") (7 "Bowler" "Black") (42 "FlatCap" "Green"))))
             (r2 (make-rel (make-heading (map make-attr '(Cap Colour Size Weight)
                                                '(string string real integer)))
                             '(("Bowler" "Black" 88.8 9) ("Bowler" "Pink" 11.0 2) ("FlatCap" "Red" 89.8 23)
                               ("FlatCap" "Red" 123.4 99) ("FlatCap" "Red" 1000.4 900)))))
        (display (expr->string (join r1 r2)))))

  (define (tuple-eq? tuple1 tuple2)
      ;;(display (string-append "comparing tuples for equality :"
      ;;                        (tuple->string tuple1) " == "
      ;;                        (tuple->string tuple2) "\n"))
      (let ((heading1 (tuple/heading tuple1))
            (heading2 (tuple/heading tuple2)))
        (if (heading-eq? heading1 heading2)
            (user-value-lists-equal? (map attr/type (heading/attrs heading1))
                                     (tuple/body tuple1)
                                     (tuple/body tuple2)))))

  (define (user-values-equal? type user-val1 user-val2)
      ;; NB - an 'expr' is a perfectly valid value - (eg custom <func-invoc-repr>s...)
      (or (expr/=? user-val1 user-val2)
          (and (catalog-fn/haskell? (catalog-type/eq-fn type))
               (core-eval-indepedent-fn-with-values (catalog-type/eq-fn type) (list user-val1 user-val2)))))

  (define (user-value-lists-equal? types values1 values2)
      ;; NB - we can't 'fold' in the 'and' because it's not a fn in Scheme
      ;; .... if this were Haskell then we could indeed fold in (&&) which _is_ a fn... (laziness)
      (or (and (null? values1) (null? values2))
          (and (user-values-equal? (car types) (car values1) (car values2))
               (user-value-lists-equal? (cdr types) (cdr values1) (cdr values2)))))

  (define (eval-join rel1 rel2)
      (display (string-append "eval-join : " (expr->string rel1) (expr->string rel2) "\n"))
      (if (not (and (rel? rel1) (rel? rel2)))
          (error "join must only be called with relations" rel1 rel2)
          (join rel1 rel2)))

  (define (eval-minus rel1 rel2)
      ;; TBA - refactor this by merging with eval-union.....!!!!!!!!!!!!!!!!
      (if (not (and (rel? rel1) (rel? rel2)))
          (error "minus must only be called with relations" rel1 rel2)
          (let* ((heading1 (rel/heading rel1))
                 (heading2 (rel/heading rel2)))
            (if (not (and (heading/valid? heading1)
                          (heading/valid? heading2)
                          (heading-eq? heading1 heading2)))
                (error "minus can only be applied to two valid, equal-headed relations" rel1 rel2)
                (make-rel heading1
                          (lset-difference (lambda (b1 b2) (tuple-eq? (make-tuple heading1 b1)
                                                                      (make-tuple heading1 b2)))
                                           (rel/body rel1)
                                           (rel/body rel2)))))))

  (define (eval-union rel1 rel2)
      (if (not (and (rel? rel1) (rel? rel2)))
          (error "union must only be called with relations" rel1 rel2)
          (let* ((heading1 (rel/heading rel1))
                 (heading2 (rel/heading rel2)))
            (if (not (and (heading/valid? heading1)
                          (heading/valid? heading2)
                          (heading-eq? heading1 heading2)))
                (error "union can only be applied to two valid, equal-headed relations" rel1 rel2)
                (make-rel heading1
                          (lset-union (lambda (b1 b2) (tuple-eq? (make-tuple heading1 b1)
                                                                 (make-tuple heading1 b2)))
                                      (rel/body rel1)
                                      (rel/body rel2)))))))

  (define (eval-projectaway rel attr-name-rel)
      ;; This code assumes that 'attr-name-rel' has a single attr "name"
      ;; eg '(projectaway parts (rel ((name) (string) (color) (size))))
      (if (not (rel? rel))
          (error "project must be called with a target relation and an attribute name relation" rel attr-name-rel)
          (begin
           (let* ((heading (rel/heading rel))
                  (attrs (heading/attrs heading))
                  (attr-names (map attr/name attrs))
                  (attr-names-to-remove (map first (rel/body attr-name-rel)))
                  (attr-names-to-keep (lset-difference eq? attr-names attr-names-to-remove))
                  (att-head (rel/heading attr-name-rel))
                  (other-attrs (filter (lambda (attr) (member (attr/name attr) attr-names-to-keep)) attrs)))
             (eval-project-to-heading rel (make-heading other-attrs))))))

  (define (eval-project rel attr-name-rel)
      ;; This code assumes that 'attr-name-rel' has a single attr "name"
      ;; eg '(project parts (rel ((name) (string) (color) (size))))
      (if (not (rel? rel))
          (error "project must be called with a target relation and an attribute name relation" rel attr-name-rel)
          (let* ((body (rel/body rel))
                 (heading (rel/heading rel))
                 (attr-names (map first (rel/body attr-name-rel)))
                 (attr-types (map (lambda (attr-name) (heading/type-for-name heading attr-name)) attr-names))
                 (name-types (zip attr-names attr-types))
                 (sorted-name-types (mergesort name-types
                                           (lambda (nt1 nt2) (string<? (symbol->string (car nt1))
                                                                       (symbol->string (car nt2))))))
                 (proj-hdg (make-heading (map make-attr (map first sorted-name-types) (map second sorted-name-types)))))
            (eval-project-to-heading rel proj-hdg))))

  (define (eval-project-to-heading rel proj-hdg)
      (let* ((heading (rel/heading rel))
             (body (rel/body rel))
             (projected-rows (map (lambda (row) (tuple/body (tuple-project (make-tuple heading row) proj-hdg))) body)))
        (make-rel proj-hdg
                  (delete-duplicates projected-rows (lambda (b1 b2) (tuple-eq? (make-tuple proj-hdg b1)
                                                                                   (make-tuple proj-hdg b2)))))))

  (define (eval-times rel1 rel2)
      ;; eg '(times rel1 rel2)
      (sexp->rel '((name) (string) (not-yet-implemented))))

  (define (eval-intersect rel1 rel2)
      ;; eg '(intersect rel1 rel2)
      (sexp->rel '((name) (string) (not-yet-implemented))))

  (define (eval-divide rel1 rel2 rel3)
      ;; eg '(divide rel1 rel2 rel3)
      (sexp->rel '((name) (string) (not-yet-implemented))))

  (define (eval-rename rel old-attr-name-ref new-attr-name-ref)
      ;; eg '(rename parts 'qty 'quantity)
      (let* ((old-rel-list (second (rel->sexp rel)))
             (old-attr-names (first old-rel-list))
             (old-attr-name (var-ref/name old-attr-name-ref))
             (new-attr-name (var-ref/name new-attr-name-ref))
             (new-attr-names (map (lambda (name) (if (eq? name old-attr-name) new-attr-name name)) old-attr-names))
             (new-rel-list (cons new-attr-names (cdr old-rel-list))))
        (sexp->rel (format-list-for-relation new-rel-list))))

  (define (eval-restrict when rel bool-expr . env)
      ;; eg '(restrict parts (= color red))
      (if (not (rel? rel))
          (error "restrict must be called with a relation and a boolean expr" rel bool-expr)
          (let* ((body (rel/body rel))
                 (heading (rel/heading rel)))
            (make-rel heading
                      (filter (lambda (row) (tuple-satisfies (make-tuple heading row) when bool-expr env))
                              body)))))

  (define (eval-summarize when rel bool-expr . env)
      ;; eg '(summarize parts part-types (rel ((expr name) (expr string) ((max weight) max-weight) ((sum weight) total-weight))))
      (sexp->rel '((name) (string) (not-yet-implemented))))

  (define (eval-extend when rel expr-rel . env)
      ;; This code assumes that 'expr-rel' has two attrs "expr" and
      ;; "name" (which will be in that order in the internal representation)
      (fold (lambda (expr-tuple-body rel-so-far)
              (let ((expr (first expr-tuple-body))
                    (new-attr-name (symbol->string (second expr-tuple-body))))
                (eval-extend-simple when rel-so-far new-attr-name expr)))
            rel
            (rel/body expr-rel)))

  (define (eval-extend-simple when rel new-attr-name-str expr . env)
      ;; eg '(extend parts calcdfigure (* weight 2.0))
      (if (not (string? new-attr-name-str))
          (error "extend: new-attr-name must be a string..." rel new-attr-name-str expr)
          ;; (read (open-input-string new-attr-name-str))
          (let ((new-attr-name (string->symbol new-attr-name-str)))
            (display "TBA - Get rid of all the nasty mergesort / quicksort nonsense and refactor it....\n")
            ;;(display (string-append (expr->string rel) (symbol->string new-attr-name) (expr->string expr)))
            ;;(newline)
            (if (not (rel? rel))
                (error "extend must be called with a relation, a col name and an expr" rel new-attr-name expr)
                (let* ((body (rel/body rel))
                       (heading (rel/heading rel))
                       (att-names (map attr/name (heading/attrs heading)))
                       (new-heading (make-heading (mergesort (cons (make-attr new-attr-name (tba-move-me-expr/type
                                                                                             expr))
                                                                   (heading/attrs heading))
                                                             (lambda (a1 a2) (string<?
                                                                              (symbol->string (attr/name a1))
                                                                              (symbol->string (attr/name a2))))))))
                  (make-rel new-heading
                            (map (lambda (row)
                                   (let* ((new-env (append (zip att-names row) env))
                                          (new-value (apply core-eval-expr when expr new-env)))
                                     (map second
                                          (mergesort (cons (list new-attr-name new-value) (zip att-names row))
                                                     (lambda (nv1 nv2) (string<? (symbol->string (car nv1))
                                                                                 (symbol->string (car nv2))))))))
                                 body)))))))

  (define (tba-move-me-expr/type expr)
      (cond ((rf-atom? expr) (cond
                               ((string? expr) 'string)
                               ((integer? expr) 'integer)
                               ((real? expr) 'real)
                               ((boolean? expr) 'boolean)
                               (else 'unknown-atom-type)))
            ((rel? expr) 'relation)
            ((quoted-expr? expr) 'expr)
            ((func-invoc-repr? expr) (catalog-fn/ret-type (func-invoc-repr/name expr)))
            (else (error "Unable to determine the type of the expr" (expr->string expr)))))

  (define (tuple-satisfies tuple when bool-expr env)
      ;; eg -   att-names : '(color weight)
      ;; eg -   att-types : '(string float)
      ;; eg - tuple-value : '(red 9.0)
      ;; eg -   bool-expr : '(> weight 8.0)
      ;;
      ;; evaluate the bool-expr in a bindings environment from the tuple...
      (let ((att-names (map attr/name (tuple/attrs tuple)))
            (body (tuple/body tuple)))
        (apply core-eval-expr
               when
               bool-expr
               (append (zip att-names body) env))))

  (define (eval-indepedent-fn-with-values fname value-list)
      ;; NB - the elements of the value-list are _NOT_ evaluated any further
    (apply (case fname
             ((union) eval-union)
             ((minus) eval-minus)
             ((join) eval-join)
             ((project) eval-project)
             ((projectaway) eval-projectaway)
             ((times) eval-times)
             ((intersect) eval-intersect)
             ((divide) eval-divide)
             ((rename) eval-rename)
             (else (error "Rel evaluator cannot evaluate independent fn" fname)))
           value-list))

  (define (eval-context-fn-with-values when fname value-list . optional-env)
      ;; context-fns are those that will need to evaluate (sub-)expressions
      ;; as part of their own evaluation
    (apply (case fname
             ((extend) eval-extend)
             ((restrict) eval-restrict)
             ((summarize) eval-summarize)
             (else (error "Rel evaluator cannot evaluate context fn" fname)))
           when
           (append value-list optional-env)))

  ;; The following is now fully supported...
  ;; (extend suppliers (rel ((expr name) (expr string) ((count (restrict (rename shipments 's_ 'x) '(= x s_))) np))))

  ;; Register with the core when the module is loaded...
  (core-register core-register-independent-fn eval-indepedent-fn-with-values '(union minus join project projectaway times intersect divide rename))
  (core-register core-register-context-fn eval-context-fn-with-values '(extend restrict summarize))
)
