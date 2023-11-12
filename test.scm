(include "syntax.scm")
(include "exception.scm")
(include "jazz.scm")
(include "math.scm")
(include "uuid.scm")
(include "object.scm")
(include "version.scm")
(include "settings.scm")
(include "network.scm")
(include "ior.scm")
(include "proxy.scm")
(include "register.scm")
(include "transmission.scm")
(include "presence.scm")
(include "remotable.scm")
(include "stubs.scm")

(exit)

(pp *presence-code*)
(pp *presence-version*)

(pp (make-uuid))

(pp (new-ior #f (make-uuid) 'interface #f '()))

(define local
  (new-local-proxy #f #f))

(define remote
  (new-remote-proxy #f #f #f))

(pp (list 'local local (proxy-live? local)))
(pp (list 'remote remote (proxy-live? remote)))

(define p
  (new-presence #f))

(pp p)
