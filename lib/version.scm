;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Version
;;;


(define (marshall-version version)
  (serialize-object 'version
                    (vector (version-major version)
                            (version-minor version)
                            (version-revision version)
                            (version-build version)
                            (version-stage version))))


(define (unmarshall-version content)
  (new-ior (vector-ref content 0)
           (vector-ref content 1)
           (vector-ref content 2)
           (vector-ref content 3)
           (vector-ref content 4)))


(define-type-of-object version
  major
  minor
  revision
  build
  stage)


(define (new-version major minor #!optional (revision 0) (build 0) (stage #f))
  (let ((version
          (make-version
            'version
            #f
            major
            minor
            revision
            build
            stage)))
    (setup-version version)
    version))


(define (setup-version version)
  (setup-object version))


(define (version=? x y)
  (and (= (version-major x) (version-major y))
       (= (version-minor x) (version-minor y))
       (= (version-revision x) (version-revision y))
       (= (version-build x) (version-build y))))


(define (version-print version output readably)
  (format output "~{{a} {a}}"
    (object-class version)
    (present-string version)))


(define (version-present-string version)
  (format "{a} {a}{?: {a}~}{?: {a}~}{?: {a}~}"
          major
          minor
          (or (not (= build 0)) (not (= revision 0))) revision
          (not (= build 0)) build
          stage stage))


(define (version-present version)
  (if (and (= major -1)
           (= minor -1))
      "Unknown"
    (format "{a}.{a}{a}{a}{a}"
            major
            minor
            (if (and (= build 0) (= revision 0))
                ""
              (format ".{a}" revision))
            (if (= build 0)
                ""
              (format ".{a}" build))
            (if (not stage)
                ""
              (format " {a}" stage)))))


(define (version->bytes version proc)
  (proc (version-major version)
        (version-minor version)
        (version-revision version)
        (version-build version)))


(define (bytes->version major minor revision build)
  (new-version major minor revision build))
