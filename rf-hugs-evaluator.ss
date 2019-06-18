;; This is the RF Hugs Evaluator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLT-isms are marked with "; PLT"

(module rf-hugs-evaluator mzscheme
  (require (lib "list.ss" "srfi" "1") ; NB - Not 100% pure SRFI-1 - name clashes given "s:" prefix
           (lib "9.ss" "srfi")          ; Record Types SRFI
           ;(lib "13.ss" "srfi")         ; string libraries - string-prefix?...
           (lib "file.ss")              ; make-temporary-file ; PLT
           (lib "process.ss"))          ; process* ; PLT
  (require "rf-core.ss")                ; core-add-evaluator - to provide eval service
                                        ; core-eval-expr - to query for functions / types
                                        ; core-monitor-expr - to monitor fns / types
  (require "rf-base.ss")
  (require "rf-expr-defs.ss")
  (require "rf-catalog.ss")             ; catalog-fn/code  catalog-fn/haskell?  catalog-function-names ...

  (define HUGS-PROMPT "Main> ")
  (define LAUNCH-HUGS-CMD "/usr/local/bin/hugs")

  (define *hugs* null)

  ;; A Hugs connection (in / out) are from the point of view of _this_ process
  (define-record-type :hugs
      (make-hugs inp out pid err ctrl)
    hugs?
    (inp hugs/in)
    (out hugs/out)
    (pid hugs/pid)
    (err hugs/err)
    (ctrl hugs/ctrl))

  (define (start-hugs file)
      ;; This proc assumes that hugs isn't already running...
      (set! *hugs* (apply make-hugs (process* LAUNCH-HUGS-CMD file))) ; PLT
                                        ; Now we need to skip past all the start-up messages until
                                        ; we get to the start prompt...
    (read-from-port-until-string (hugs/in *hugs*) HUGS-PROMPT))

  (define (stop-hugs)
      (close-output-port (hugs/out *hugs*))
    (close-input-port (hugs/in *hugs*))
    (close-input-port (hugs/err *hugs*))
    (hugs/ctrl 'kill)
    (set! *hugs* null))

  (define (hugs-is-running) (not (null? *hugs*)))
  (define (hugs-start-if-necessary) (if (not (hugs-is-running)) (hugs-reconfigure)))

  (define (hugs-reconfigure) ; With the latest types / fns etc...
      ;; First we need to prep an input file to give to hugs
      (let* ((file (make-temporary-file))) ; PLT
        (hugs-write-defs (open-output-file file 'truncate/replace)) ; PLT
        (if (hugs-is-running) (stop-hugs))
        (start-hugs file)))

  (define (hugs-write-defs out-port)
      (map (lambda (fname)
             (display (string-append "-- " (symbol->string fname) "\n"
                                     (catalog-fn/code fname) "\n\n")
                      out-port))
           (filter catalog-fn/haskell? (catalog-function-names))))

  (define (prepare-arg-for-hugs arg)
      (string-append " " (stringify arg)))

  (define (eval-indepedent-fn-with-values fname value-list)
;;      (display (string-append "hugs : " (symbol->string fname) " "
;;                              (apply string-append (map expr->string value-list)) "\n"))
      (hugs-eval (apply string-append (cons (symbol->string fname) (map prepare-arg-for-hugs value-list)))))

  (define (hugs-eval expr-string)
      ;; Evaluate the expr-string _synchronously_
      ;;(display "hugs-eval:") (display expr-string) (newline)
      (hugs-start-if-necessary)
      (display expr-string (hugs/out *hugs*))
      (newline (hugs/out *hugs*))
      (let ((res (read-from-port-until-prompt (hugs/in *hugs*) HUGS-PROMPT)))
        (hugs-to-expr res expr-string)))

  (define (hugs-to-expr hugs-val expr-for-diagnostic)
      (let* ((p (open-input-string hugs-val))
             (raw-value (read p)))      ; By passing straight to (read) we may be losing case info....
        (cond
          ((symbol? raw-value) (case raw-value
                                 ((true) #t)
                                 ((false) #f)
                                 ((error) (error "Hugs raised an error whilst evaluating expr" expr-for-diagnostic))
                                 (else
                                  (symbol->string raw-value))))
          ;((string? raw-value) raw-value) ; TBA - need to parse constructors into :func-invoc-reprs here....
          ((integer? raw-value) raw-value)
          ((real? raw-value) raw-value)
          (else (error "Unknown hugs value - failed trying to convert to internal repr" hugs-val)))))

  ;; Register with the core when the module is loaded...
  (core-register-default-independent-evaluator eval-indepedent-fn-with-values)
)
