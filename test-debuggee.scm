(include "syntax.scm")
(include "exception.scm")
(include "jazz.scm")
(include "math.scm")
(include "uuid.scm")
(include "object.scm")
(include "version.scm")
(include "settings.scm")
(include "network.scm")
(include "listener.scm")
(include "ior.scm")
(include "proxy.scm")
(include "register.scm")
(include "transmission.scm")
(include "presence.scm")
(include "stubs.scm")
(include "debuggee.scm")

(set! ##primordial-exception-handler-hook
  (lambda (exc other)
    (if (connection-broke-exception? exc)
        (exit)
      (##repl-exception-handler-hook exc other))))

(start-presence purpose: 'debugging)

(setup-local-process)

(thread-sleep! +inf.0)
