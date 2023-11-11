(include "syntax.scm")
(include "exception.scm")
(include "jazz.scm")
(include "math.scm")
(include "uuid.scm")
(include "version.scm")
(include "settings.scm")
(include "network.scm")
(include "listener.scm")
(include "ior.scm")
(include "proxy.scm")
(include "register.scm")
(include "transmission.scm")
(include "presence.scm")

(define p
  (new-remote-presence #f))

(start-remote-presence)

(thread-sleep! +inf.0)
