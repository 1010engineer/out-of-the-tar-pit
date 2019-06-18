;; This is the RF Catalog
;; it encapsulates all of the system internals and catalog structure
;; behind a set of fns....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLT-isms are marked with "; PLT"

(module rf-catalog mzscheme
  (require (lib "list.ss" "srfi" "1"))  ; for lset-union...
  (require "rf-core.ss")                ; core-add-evaluator - to provide eval service
                                        ; core-eval-expr - to query for functions / types
                                        ; core-monitor-expr - to monitor fns / types

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; R-Functions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; The support for user-level functions (R-Functions) (Relational fns)
  ;; Externally to this module fns are represented as _symbols_ - ie the interned
  ;; version of their name string
  ;;
  ;; Broadly speaking there are thress types of fn:

  ;; 1) Built-in
  ;; These consist of relational operators and other common fns
  ;; The relational operators are those from the relational
  ;; algebra - they return relations as their result
  ;; There is an _implicit_ ie well-known, commonly-agreed-upon
  ;; meaning for what these functions need to do. As such
  ;; their is no representation of their internals

  ;; 2) Constructors
  ;; Even though these are conceptually a fn, they often won't
  ;; need to be evaluated and can be returned to a client in a result
  ;; They parallel the Haskell concept of constructors

  ;; 3) User Functions (ie Haskell fns)
  ;; These are functions whose code is written by the user

  (define (catalog-fn? fname) (or (catalog-fn/builtin? fname)
                                  (catalog-fn/constructor? fname)
                                  (catalog-fn/haskell? fname)))
  (provide catalog-fn?)

  (define (catalog-fn/builtin? fname) (member fname (builtin-function-names)))
  (provide catalog-fn/builtin?)

  (define (catalog-fn/constructor? fname) (member fname (constructor-function-names)))
  (provide catalog-fn/constructor?)

  (define (catalog-fn/haskell? fname) (member fname (haskell-function-names)))
  (provide catalog-fn/haskell?)

  (define (builtin-function-names) '(restrict join project projectaway union minus extend
                                     times intersect divide summarize rename
                                     count sum max min avg all any
                                     + - * / > < >= <= not))
  (define (constructor-function-names) '())
  (define (haskell-function-names) '(benless benfunc benfib benadd1))

  (define (catalog-function-names) (lset-union eq?
                                               (builtin-function-names)
                                               (constructor-function-names)
                                               (haskell-function-names)))
  (provide catalog-function-names)

  (define (catalog-fn/code fname)
      (if (catalog-fn/haskell? fname)   ; TBA - throw an exception otherwise
          (case fname
            ((benless) "benless a b = a < (b + 1)\n")
            ((benfunc) (string-append "benfunc 0 = 1\n"
                                    "benfunc x = x * benfunc (x - 1)\n"))
            ((benfib) (string-append "benfib 0 = 1\n" ;
                                   "benfib 1 = 1\n"
                                   "benfib n = benfib (n - 1) + benfib (n - 2)\n"))
            ((benadd1) (string-append "benadd1 n = 1 + n\n"))
            (else (error "attempt to get code from codeless function" fname)))))
  (provide catalog-fn/code)

  (define (catalog-fn/ret-type fname)
      (case fname
        ((benless) 'boolean)
        ((benfunc benfib benadd1) 'integer)
        ((count) 'integer)
        ((sum max min avg) 'real)
        ((all any) 'boolean)
        ((minus restrict union extend join times intersect divide summarize project projectaway rename) 'rel)
        ((+ * / -) 'real)
        ((< > <= >= not) 'boolean)
        (else (error "attempt to get ret-type from unknown fn" fname))))
  (provide catalog-fn/ret-type)

  (define (catalog-fn/arg-types fname)
      (case fname
        ((benless) '(integer integer))
        ((benfunc benfib benadd1) '(integer))
        ((count) '(rel))
        ((sum max min avg all any) '(rel string))
        ((not) '(boolean))
        ((union minus join times intersect) '(rel rel))
        ((restrict) '(rel expr))
        ((project projectaway) '(rel rel))
        ((extend) '(rel string expr))
        ((divide) '(rel rel rel))
        ((summarize) '(rel rel rel))
        ((rename) '(rel string string))
        ((+ * / - < > <= >=) '(real real))
        (else (error "attempt to get arg-types from unknown fn" fname))))
  (provide catalog-fn/arg-types)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Types
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; The support functions for types at the user level
  ;; Externally to this module types are represented as _symbols_ - ie the interned
  ;; version of their name string

  ;;    (define (type-names)
  ;;        (core-eval-expr when (make-func-invoc-repr (make-rel-operator tba-should be just name?) ;func
  ;;                                              (list "")))) ; args

  (define (catalog-type? tname) (or (catalog-type/ordinal? tname) (catalog-type/non-ordinal? tname)))
  (provide catalog-type?)

  ;;(define (catalog-type-names) (core-eval-expr when (make-var-ref "TYPES"))) ; args
  ;;(define (catalog-type-names) (lset-union (ordinal-type-names) (non-ordinal-type-names)))
  ;; NB this is a private fn... so it is not (provide)ed

  (define (ordinal-type-names) '(integer real string))
  (define (non-ordinal-type-names) '(boolean expr))

  (define (catalog-type/ordinal? tname) (member tname (ordinal-type-names)))
  (provide catalog-type/ordinal?)

  (define (catalog-type/non-ordinal? tname) (member tname (non-ordinal-type-names)))
  ;; NB this is a private fn... so it is not (provide)ed

  (define (catalog-type/eq-fn tname)
      (case tname
        ((integer) 'integer-eq)
        ((real) 'real-eq)
        ((string) 'string-eq)
        ((boolean) 'boolean-eq)
        ((when) 'when-eq)
        ((expr) 'expr-eq)               ; NB expr equality is a totally different thing from
                                        ; expr-_value_ equality!
        (else (error "attempt to get eq-fn for unknown type" tname))))
  (provide catalog-type/eq-fn)

  (define (catalog-type/ord-fn tname)
      (if (catalog-type/ordinal? tname) ; TBA - exception if non-ordinal
          (case tname
            ((integer) 'integer-gt)
            ((real) 'real-gt)
            ((string) 'string-gt)
            ((when) 'when-gt)
            (else (error "attempt to get ord-fn for non-ordinal type / type without registered ord-fn" tname)))))
  (provide catalog-type/ord-fn)

  ;; [each type has an (implicit) relationship to many 'pieces'
  ;;  the pieces are _not_ stored with/inside the type because
  ;;  that would be redundant (each piece knows what its 'type'
  ;;  is) - so we can infer the pieces which correspond to a type]

  ;; The piece type
  ;; NB - A 'piece' is the analogue of a 'constructor' in Haskell ie it
  ;; isn't a type in its own right, and _all_ values can be considered
  ;; to be - at least implicitly - tagged with their _piece_ (not their
  ;; type) - this is what allows constructor-based pattern matching
  ;; to work. Anyway, a piece 'belongs' to one - and only one - type.
  ;;
  ;; Piece [ name, type, constructor, selectors ]


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Other
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Catalog
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Catalog
  ;; ORDINAL_TYPE [name <String>, eq_fn_name <String>, ord_fn_name <String>]
  ;; NON_ORDINAL_TYPE [name <String>, eq_fn_name <String>]
  ;; FUNCTION [name <String>, ret_type_name <String>, arg_types <StringList>, ftyp <CTR|B-IN|REL-OP>]
  ;; USER_FUNCTION [name <String>, code <String>, language_fk? <Int>]
  ;; NB - User fns need to be able to be constructors too
  ;;
  ;; Initial Ordinal Types
  ;; ["String", "string-eq", "string-gt"]
  ;; ["Int", "int-eq", "int-gt"]
  ;;
  ;; Non-Ordinal Types
  ;; ["Boolean", "bool-eq"]

)