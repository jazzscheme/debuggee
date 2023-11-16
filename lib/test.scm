#! gsi-script

(##import all)

(start-presence purpose: 'debugging)

(setup-local-process)

(setup-debuggee)
(ready-debuggee)

(current-process-title-set! "Welcome to Scheme!")

(start-repl)
