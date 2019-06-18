;; This is the RF server
;;;;;;;;;;;;;;;;;;;;;;;;
;; export PATH=$PATH:/LocalApps/PLT/bin
;; mzscheme -r rf-server.ss
;; PLT-isms are marked with "; PLT"

(require "rf-base.ss")
(require "rf-core.ss") ;; rf-core: runloop

(display "RF Server starting...\n\n")

;; Load the components...
;; ...this will cause them to register with the rf-core...
(require "rf-rel-evaluator.ss"
         "rf-builtin-evaluator.ss"
         "rf-store.ss"
         "rf-hugs-evaluator.ss"
         "rf-client.ss")

(runloop)                             ; provided by the core

;; TODO...
;; Add 'expressionOC' outlet to XXDisplayGroup... or a custom FilesOwnerClass with a "displayGroups" binding (so that you can bind controllers to FilesOwner.displayGroup.parts etc? ...and add a "keys" binding to the displayGroup that will manage a collection of string substitutions to put into the real expr (remember to use an NSDict wrapper...).  Using the Files Owner may be better than doing the reverse binding technique used at present because then you can control the binding parameters in IB more easily... for the DG contents, for the expression and for the "keys" used to parameterize the expression.
