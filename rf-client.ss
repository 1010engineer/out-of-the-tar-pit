;; This is the RF Client
;; It implements the rf-client^ sig and passes
;; messages from clients to the core and returns
;; the responses to the clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLT-isms are marked with "; PLT"

(module rf-client mzscheme
  (require (lib "list.ss" "srfi" "1")   ; NB - Not 100% pure SRFI-1 - name clashes given "s:" prefix
           (lib "time.ss" "srfi" "19")  ; Date / Time SRFI ; with renames (prefix "srfi:")
                                        ; where clashes occur - srfi:make-date, srfi:date? ...
                                        ; beware "date?" will be the MzScheme one....
                                        ; We mostly just use the SRFI 19 "time" structure though
           (lib "11.ss" "srfi")         ; let-values
           (lib "9.ss" "srfi"))         ; Record Types SRFI
  (require "rf-core.ss")                ; core-eval-expr - to run queries for clients
                                        ; core-monitor-expr - to update queries for clients
                                        ; core-add-to-runloop - to receive client input
  (require "rf-base.ss")                ; stringify etc...
  (require "rf-expr-defs.ss")

  (define SERVICE-PORT 18888)
  (define SERVER-HOST "localhost")

  (define *connected-clients* '())

  ;; The client connection type
  ;; (in / out) are from the point of view of _this_ process
  (define-record-type :ccon
      (make-ccon inp out msg)
    ccon?
    (inp ccon/input-sock)
    (out ccon/output-sock)
    (msg ccon/msg-so-far ccon/set-msg-so-far!))

  (define (ccon/msg-ready? ccon)
      ;; returns yes if there's an equal number of open and
      ;; close parens - and there's more than one...
      (let* ((msg-so-far (ccon/msg-so-far ccon))
             (chars (string->list msg-so-far))
             (num-opens  (length (filter (lambda (c) (eq? c #\()) chars)))
             (num-closes (length (filter (lambda (c) (eq? c #\))) chars))))
        (and (= num-opens num-closes)
             (> num-opens 0))))

  (define (process-request ccon rid msg)
      ;; This is the proc which processes a message once it has been
      ;; received from the client and returns the response
      ;;(display (string-append "process-request called..." (stringify rid) " " (stringify msg) "\n"))
      (let ((response
             (if (pair? msg)
                 (case (car msg)
                   ((debug) (eval (second msg) (interaction-environment)))
                   ((monitor)
                      (core-monitor-expr (sexp->expr (second msg) make-var-ref) ; expr
                                         (cons ccon rid)      ; context - so we can stop monitoring later...
                                         (lambda (new-val) (send-response rid new-val ccon)))
                      (client-eval-msg (second msg))) ; eval the bit inside the monitor straight away
                   ((assign)
                      (core-assign (current-time TIME-UTC) ; when 'new value expr' and 'precond' should be eval'd..
                                   (sexp->expr (second msg) make-var-ref)  ; precond
                                   (third msg)                             ; variable-name
                                   (sexp->expr (fourth msg) make-var-ref)) ; new value expression
                      77)
                   (else (client-eval-msg msg))) ; presumably some kind of func-invoc..
                 (case msg              ; msg is an atom...
                   ((close) (close-connection ccon))
                   ((stop) (stop-monitoring ccon rid))
                   (else    (client-eval-msg msg)))))) ; presumably a straight value or var-ref
        (send-response rid response ccon)))

  (define (client-eval-msg msg)
      (let ((expr (sexp->expr msg make-var-ref)))
        (if (expr/has-free-vars? null expr)
            (begin
             ;; TBA - set up the callbacks...
             ;; using (core-monitor-expr) .... if we do this then we could
             ;; get rid of the explicit "monitor" declaration / request
             (core-eval-expr (current-time TIME-UTC) expr))
            (core-eval-expr null expr))))

  (define (close-connection ccon)
      (display "Closing client connection....\n")
    (core-remove-from-runloop (ccon/input-sock ccon))
    (close-output-port (ccon/output-sock ccon))
    (close-input-port (ccon/input-sock ccon)))

  (define (stop-monitoring ccon rid) (core-stop-monitoring (cons ccon rid)))

  (define (read-rid-and-msg ccon)
      (let* ((p (open-input-string (ccon/msg-so-far ccon)))
             (msg (read p))) ; Read one sexp
        (display (string-append "Msg received: " (stringify msg) "\n"))
        (ccon/set-msg-so-far! ccon (read-from-port-no-block p)) ; Put the remainder back
        (if (not (and (pair? msg) (= (length msg) 2)))
            (error "Invalid message received from client. Messages must be sent as a 2-elt list (rid body)" msg)
            (values (first msg) (second msg)))))

  (define (send-response rid response ccon)
      (if (not (void? response))
          (let ((resp-string (string-append (stringify `(,rid ,(expr->sexp response))) "\n")))
            (display (string-append "About to send resp:" resp-string "\n"))
            (display resp-string (ccon/output-sock ccon)))))

  (define (process-connection ccon)
      ;; Read and process all input from 'ccon' until exhausted...
      ;; This proc is not supposed to return anything....
      (with-handlers ((exn:i/o:port:closed? ; PLT
                       (lambda (exn)
                         (display "Cleaning up closed client connection...")
                         (close-connection ccon))))
        (let ((next-msg-bit (read-from-port-no-block (ccon/input-sock ccon))))
        ;;(display (string-append "RAW: [" next-msg-bit "]\n"))
        (ccon/set-msg-so-far! ccon (string-append (ccon/msg-so-far ccon) next-msg-bit))
        (if (ccon/msg-ready? ccon)
            (begin
             (let-values (((rid msg) (read-rid-and-msg ccon)))
                         (process-request ccon rid msg))
             ;; Now recurse to process any other messages still waiting on this connection
             (process-connection ccon))))))

  (define (accept-connection listener)
      (display " ...client module notified of new connection...\n")
    (if (tcp-accept-ready? listener) ; PLT
        (let*-values
         (((client->me me->client) (tcp-accept listener))
          ((ccon) (make-ccon client->me me->client ""))) ; PLT
         (set! *connected-clients* (cons ccon *connected-clients*))
         (core-add-to-runloop client->me (lambda () (process-connection ccon))))))

  ;; Register etc
  (let ((listener (tcp-listen SERVICE-PORT 4 #t))) ; #t to reuse addr... ; PLT
    (display "Client module started listening on port...")
    (display SERVICE-PORT)
    (newline)
    (core-add-to-runloop listener (lambda () (accept-connection listener)))
    (core-register-shutdown-hook (lambda ()
                                   (map (lambda (con)
                                          (close-output-port (ccon/output-sock con))
                                          (close-input-port (ccon/input-sock con)))
                                        *connected-clients*))))
)
