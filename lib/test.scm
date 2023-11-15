#! gsi-script

(##import all)

(start-presence purpose: 'debugging)

(setup-local-process)

(attach-debuggee "localhost" 56000)
(ready-debuggee)

(current-process-title-set! "Welcome to Scheme!")

(import (api))

(start-repl)
