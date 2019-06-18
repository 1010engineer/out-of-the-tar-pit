;; This is RF Base
;; It provides utility functions used generally throughout the system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLT-isms are marked with "; PLT"

(module rf-base mzscheme
;;   (require (lib "time.ss" "srfi" "19")) ; Date / Time SRFI ; with renames (prefix "srfi:")
;;                                         ; where clashes occur - srfi:make-date, srfi:date? ...
;;                                         ; beware "date?" will be the MzScheme one....
;;                                         ; We mostly just use the SRFI 19 "time" structure though
  (require (lib "6.ss" "srfi")) ; String Ports
  (require (lib "13.ss" "srfi")) ; string libraries

  (define (stringify obj)
      (let ((string-port (open-output-string)))
        (write obj string-port)
        (get-output-string string-port)))
  (provide stringify)

  (define (read-from-port-until-prompt p prompt)
      (let* ((res (read-from-port-until-string p prompt))
             (lenres (string-length res))
             (lenprompt (string-length prompt)))
        (if (<  lenres lenprompt)
            res
            (substring res 0 (- lenres lenprompt)))))
  (provide read-from-port-until-prompt)

  (define (read-from-port-until-string p terminator)
      (read-from-port-until p
                            (lambda (p string-so-far)
                              (string-suffix? terminator string-so-far))
                            ""))
  (provide read-from-port-until-string)

  (define (read-from-port-no-block p)
      ;; Will return empty string if there's nothing to read
      (read-from-port-until p
                            (lambda (p string-so-far)
                              (not (with-handlers ((exn:i/o:port:closed? ; v209 PLT
;;                              (not (with-handlers ((exn:fail:network? ; v300 PLT
                                                    (lambda (exn) "")))
                                     (char-ready? p))))
                            ""))
  (provide read-from-port-no-block)

  (define (read-from-port-until p stop? msg-so-far)
      ;; Returns the string that was read & closes if the end is reached
      (if (stop? p msg-so-far)
          msg-so-far
          (let ((c (read-char p)))
            (if (eof-object? c)
                (begin (close-input-port p) msg-so-far)
                (read-from-port-until p
                                      stop?
                                      (string-append msg-so-far (string c)))))))
  (provide read-from-port-until)

)
