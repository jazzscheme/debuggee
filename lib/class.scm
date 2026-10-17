;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Classes
;;;


(define-type class
  extender: define-type-of-class
  name
  methods)


(define (setup-class class)
  (class-methods-set! class (make-table)))


(define (class-add-method class method-name method)
  (table-set! (class-methods class) method-name method))


(define (class-dispatch obj method-name)
  (let ((class (class-of obj)))
    (let ((method (table-ref (class-methods class) method-name #f)))
      (if (not method)
          (error "Unable to find method" method-name)
        method))))


(define (class-of obj)
  (or (find-class obj)
      (error "Unable to get class of" obj)))


(define (find-class-name obj)
  (let ((class (find-class obj)))
    (if class
        (class-name class)
      '<unknown>)))


(define (find-class obj)
  (or (sequence-class obj)
      (cond ((boolean? obj)      Boolean)
            ((char? obj)         Char)
            ((fixnum? obj)       Fixnum)
            ((flonum? obj)       Flonum)
            ((integer? obj)      Integer)
            ((rational? obj)     Rational)
            ((real? obj)         Real)
            ((complex? obj)      Complex)
            ((number? obj)       Number)
            ((null? obj)         Null)
            ((pair? obj)         Pair)
            ((table? obj)        Table)
            ((string? obj)       String)
            ((symbol? obj)       Symbol)
            ((keyword? obj)      Keyword)
            ((port? obj)         Port)
            ((continuation? obj) Continuation)
            ((procedure? obj)    Procedure)
            ((foreign? obj)      Foreign)
            ((values? obj)       Values)
            ((eof-object? obj)   EOF)
            ((unspecified? obj)  Unspecified)
            (else                #f))))


(define (sequence-class obj)
  (cond ((vector? obj)       Vector)
        ((s8vector? obj)     S8Vector)
        ((u8vector? obj)     U8Vector)
        ((s16vector? obj)    S16Vector)
        ((u16vector? obj)    U16Vector)
        ((s32vector? obj)    S32Vector)
        ((u32vector? obj)    U32Vector)
        ((s64vector? obj)    S64Vector)
        ((u64vector? obj)    U64Vector)
        ((f32vector? obj)    F32Vector)
        ((f64vector? obj)    F64Vector)
        (else                #f)))


(define Boolean      (make-class 'Boolean #f))
(define Char         (make-class 'Char #f))
(define Fixnum       (make-class 'Fixnum #f))
(define Ratnum       (make-class 'Ratnum #f))
(define Flonum       (make-class 'Flonum #f))
(define Integer      (make-class 'Integer #f))
(define Rational     (make-class 'Rational #f))
(define Real         (make-class 'Real #f))
(define Complex      (make-class 'Complex #f))
(define Number       (make-class 'Number #f))
(define Null         (make-class 'Null #f))
(define Pair         (make-class 'Pair #f))
(define Table        (make-class 'Table #f))
(define String       (make-class 'String #f))
(define Symbol       (make-class 'Symbol #f))
(define Keyword      (make-class 'Keyword #f))
(define Port         (make-class 'Port #f))
(define Continuation (make-class 'Continuation #f))
(define Procedure    (make-class 'Procedure #f))
(define Foreign      (make-class 'Foreign #f))
(define Values       (make-class 'Values #f))
(define EOF          (make-class 'EOF #f))
(define Unspecified  (make-class 'Unspecified #f))
(define Marker       (make-class 'Marker #f))
