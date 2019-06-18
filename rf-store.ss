;; This is the RF Store
;; - this is an evaluation module that will interpret <var-ref>s as
;; stored rel-var refs (ie tables) if they are not bound in the env
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLT-isms are marked with "; PLT"

(module rf-store mzscheme
  (require (lib "list.ss" "srfi" "1"))  ; NB - Not 100% pure SRFI-1 - name clashes given "s:" prefix
  (require (lib "13.ss" "srfi")) ; string libraries
  (require (lib "time.ss" "srfi" "19")) ; Date / Time SRFI ; with renames (prefix "srfi:")
                                        ; where clashes occur - srfi:make-date, srfi:date? ...
                                        ; beware "date?" will be the MzScheme one....
                                        ; We mostly just use the SRFI 19 "time" structure though
  ;;(require (rename (lib "list.ss" "mzlib") quicksort quicksort)) ; PLT
  (require "rf-base.ss")                ; stringify
  (require "rf-core.ss")                ; core-add-evaluator
  (require "rf-expr-defs.ss")

  ;;(define DATA-DIR "/Users/ben/Documents/OOTP/Scheme/data/emarket")
  (define DATA-DIR "/Users/ben/Documents/OOTP/Scheme/data/trader")
  ;;(define DATA-DIR "/Users/ben/Documents/OOTP/Scheme/data/test")

  (define (save-relation rel-var-name new-rel)
      (display (string-append "saving table....:" (symbol->string rel-var-name) "\n"))
     (let* ((new-sexp-value (second (rel->sexp new-rel)))
            (dir-path (dir-for-table rel-var-name))
            (now-time (current-time TIME-UTC))
            (path (build-path dir-path
                              (string-append (symbol->string rel-var-name) "$"
                                             (stringify (time-second now-time)) "-"
                                             (stringify (time-nanosecond now-time)) ".data")))) ; PLT
       (if (not (directory-exists? dir-path))
           #f
           (begin
            ;; (display (string-append "save-relation[" (date->string (time-utc->date now-time) "~T ~N") "]...path=" path "\n"))
            (write-to-file path new-sexp-value)
            #t))))

  (define (write-to-file filename new-value)
      (call-with-output-file filename
                             (lambda (p)
                               (map (lambda (x) (write x p) (newline p)) new-value))))

  (define (dir-for-table table-name) (build-path DATA-DIR (symbol->string table-name))) ; PLT

  (define (read-from-file filename)
      ;; Adapted from the Scheme Cookbook
      (call-with-input-file filename
                            (lambda (p)
                              (let loop ((line (read p))
                                         (result '()))
                                   (if (eof-object? line)
                                       (reverse result)
                                       (loop (read p) (cons line result)))))))

  (define (lookup-relation when table-name)
      ;; "when" will be a SRFI 19 "time" structure with UTC base.
      ;;(display (string-append "store::  " (date->string (time-utc->date when)) " " (symbol->string table-name) "\n"))
      (let ((dir (dir-for-table table-name)))
        (if (directory-exists? dir) ; PLT
            (let* ((potential-versions (directory-list dir))
                   (versions (filter (lambda (x) (string-suffix? ".data" x) dir) potential-versions))
                   (path (build-path dir (version-to-load when versions null dir)))) ; PLT
;               (display (string-append "lookup-relation["
;                                       (date->string (time-utc->date when) "~T ~N")
;                                       "]...<<" (stringify versions) ">>path=" path "\n"))
              (sexp->rel (format-list-for-relation (read-from-file path))))
            (error "could not lookup non-existant relation" table-name))))

  (define (time-for-version version dir)
      ;;(display (string-append "time-for-version " version " " dir "\n"))
      (let ((ind (string-index version #\$)))
        (if ind
            (let* ((tstring (string-drop version (+ 1 ind)))
                   (ind2 (string-index tstring #\-))
                   (nanosec-with-suffix (string-drop tstring (+ 1 ind2)))
                   (nanosec (string-take nanosec-with-suffix (string-index nanosec-with-suffix #\.)))
                   (sec (string-take tstring ind2)))
              (make-time TIME-UTC (string->number nanosec) (string->number sec)))
            ;; If no time is derivable from the filename then resort to getting
            ;; an approximate time from the file modification stamp...
            (let* ((platform-seconds-mod-time (file-or-directory-modify-seconds (build-path dir version)))
                   (plt-date (seconds->date platform-seconds-mod-time))
                   (srfi-date (make-srfi:date 0
                                         (date-second plt-date)
                                         (date-minute plt-date)
                                         (date-hour plt-date)
                                         (date-day plt-date)
                                         (date-month plt-date)
                                         (date-year plt-date)
                                         (date-time-zone-offset plt-date))))
              (date->time-utc srfi-date)))))

  (define (version-to-load when versions best-version-so-far dir)
      ;; best-version-so-far will start of as null until a valid version
      ;; is encountered...
      (if (null? versions)
          best-version-so-far
          (version-to-load when
                           (cdr versions)
                           (best-version when best-version-so-far (car versions) dir)
                           dir)))

  (define (best-version when best-version-so-far other-version dir)
      (let ((other-version-time (time-for-version other-version dir)))
        ;;(display (expr->string other-version-time)) (newline)
        (if (time<? when other-version-time)
            best-version-so-far
            ;; We know that other-version has a valid time...
            (if (null? best-version-so-far)
                other-version
                ;; We know that both times are valid... so choose the most recent...
                 (if (time<? (time-for-version best-version-so-far dir) other-version-time)
                     other-version
                     best-version-so-far)))))

  ;; Register with the core when the module is loaded...
  (core-register-stored-var-resolver (cons lookup-relation save-relation))
  ;;(display (map (lambda (v) (date->string (time-utc->date (time-for-version v (build-path DATA-DIR "")))))
  ;;              (directory-list DATA-DIR)))
)
