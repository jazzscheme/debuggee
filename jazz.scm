;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz
;;;
;;;  The contents of this file are subject to the Mozilla Public License Version
;;;  1.1 (the "License"); you may not use this file except in compliance with
;;;  the License. You may obtain a copy of the License at
;;;  http://www.mozilla.org/MPL/
;;;
;;;  Software distributed under the License is distributed on an "AS IS" basis,
;;;  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
;;;  for the specific language governing rights and limitations under the
;;;  License.
;;;
;;;  The Original Code is JazzScheme.
;;;
;;;  The Initial Developer of the Original Code is Guillaume Cartier.
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
;;;  the Initial Developer. All Rights Reserved.
;;;
;;;  Contributor(s):
;;;
;;;  Alternatively, the contents of this file may be used under the terms of
;;;  the GNU General Public License Version 2 or later (the "GPL"), in which
;;;  case the provisions of the GPL are applicable instead of those above. If
;;;  you wish to allow use of your version of this file only under the terms of
;;;  the GPL, and not to allow others to use your version of this file under the
;;;  terms of the MPL, indicate your decision by deleting the provisions above
;;;  and replace them with the notice and other provisions required by the GPL.
;;;  If you do not delete the provisions above, a recipient may use your version
;;;  of this file under the terms of any one of the MPL or the GPL.
;;;
;;;  See www.jazzscheme.org for details.


;;;
;;;; Serial
;;;


(define (object->serial obj)
  (object->serial-number obj))


(define (serial->object number)
  (serial-number->object number))


;;;
;;;; List
;;;


(define (remove item lst)
  (let iter ((scan lst))
    (if (not (null? scan))
        (let ((value (car scan)))
          (if (eq? value item)
              (iter (cdr scan))
            (cons value (iter (cdr scan)))))
      '())))


(define (list-find list target key test start return)
  (let ((key (if (not key) (lambda (x) x) key))
        (test (if (not test) eqv? test)))
    (let iter ((rest (tail list start)) (rank start))
       (cond ((null? rest) #f)
             ((test target (key (car rest)))
              (case return
                ((item) (car rest))
                (else rank)))
             (else
              (iter (cdr rest) (+ rank 1)))))))


;;;
;;;; String
;;;


(define (string-find str c #!optional (start 0))
  (let ((len (string-length str)))
    (let iter ((n start))
      (cond ((fx>= n len)
             #f)
            ((char=? (string-ref str n) c)
             n)
            (else
             (iter (fx+ n 1)))))))


;; todo
(define (upcase str)
  str)


;;;
;;;; Unspecified
;;;


(define (unspecified)
  (void))


(define (unspecified? expr)
  (eq? expr (void)))


(define (specified? expr)
  (not (eq? expr (void))))


;;;
;;;; Thread
;;;


(define pristine-thread-continuation
  (thread-join!
    (thread-start!
      (make-thread
        (lambda ()
          (continuation-capture
            (lambda (cont)
              cont)))))))


(define (exit-thread thread)
  (thread-int! thread thread-exit))


;;;
;;;; I/O
;;;


(define (write-32-bit-integer n port)
  (32-bit-integer->bytes n
    (lambda (b1 b2 b3 b4)
      (write-u8 b1 port)
      (write-u8 b2 port)
      (write-u8 b3 port)
      (write-u8 b4 port))))


(define (read-32-bit-integer port)
  (let* ((b1 (read-u8 port))
         (b2 (read-u8 port))
         (b3 (read-u8 port))
         (b4 (read-u8 port)))
    (if (eof-object? b4)
        b4
      (bytes->32-bit-integer b1 b2 b3 b4))))


(define (write-binary-content u8vect port)
  (let ((size (u8vector-length u8vect)))
    (write-32-bit-integer size port)
    (write-subu8vector u8vect 0 size port)
    (+ 4 size)))


(define (read-binary-content port)
  (let ((size (read-32-bit-integer port)))
    (if (eof-object? size)
        size
      (let ((u8vect (make-u8vector size)))
        (let ((read (read-subu8vector u8vect 0 size port)))
          (if (not (= read size))
              (eof-object)
            u8vect))))))


(define (write-binary-object data port #!optional (encoder #f))
  (let ((u8vect (if encoder
                    (object->u8vector data encoder)
                  (object->u8vector data))))
    (let ((size (u8vector-length u8vect)))
      (write-32-bit-integer size port)
      (write-subu8vector u8vect 0 size port)
      (+ 4 size))))


(define (read-binary-object port #!optional (decoder #f))
  (let ((size (read-32-bit-integer port)))
    (if (eof-object? size)
        size
      (let ((u8vect (make-u8vector size)))
        (let ((read (read-subu8vector u8vect 0 size port)))
          (if (not (= read size))
              (eof-object)
            (if decoder
                (u8vector->object u8vect decoder)
              (u8vector->object u8vect))))))))


(define (write-binary data port)
  (write-binary-object data port serialize))


(define (read-binary port)
  (read-binary-object port deserialize))


;;;
;;;; Serialize
;;;


(define-type serialized
  id: 5E781AB5-7492-4052-8E00-15B72A7A19FC
  (class read-only:)
  (content read-only:))


(define (serialize-object class content)
  (make-serialized class content))


(define (serialize obj)
  (if (object? obj)
      (or (marshall-object (object-class obj) obj)
          (error "Unable to serialize" obj))
    obj))


(define (deserialize obj)
  (if (serialized? obj)
      (let ((class (serialized-class obj))
            (content (serialized-content obj)))
        (or (unmarshall-object class content)
            (error "Unable to deserialize" class)))
    obj))


;;;
;;;; Encoding
;;;


(define (32-bit-integer->bytes x proc)
  (proc (modulo (arithmetic-shift x -24) 256)
        (modulo (arithmetic-shift x -16) 256)
        (modulo (arithmetic-shift x -8) 256)
        (modulo x 256)))


(define (bytes->32-bit-integer b1 b2 b3 b4)
  (+ (arithmetic-shift b1 24)
     (arithmetic-shift b2 16)
     (arithmetic-shift b3 8)
     b4))


(define (code-string->32-bit-integer s)
  (bytes->32-bit-integer (char->integer (string-ref s 0))
                         (char->integer (string-ref s 1))
                         (char->integer (string-ref s 2))
                         (char->integer (string-ref s 3))))
