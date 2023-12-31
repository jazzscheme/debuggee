;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Remote Transmission
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
;;;; Broke
;;;


(define Connection-Broke
  'Connection-Broke)


(define (new-connection-broke reason)
  (list Connection-Broke reason))


(define (connection-broke-exception? exc)
  (and (pair? exc)
       (eq? (car exc) Connection-Broke)))


(define (throw-connection-broke reason)
  (raise (new-connection-broke reason)))


;;;
;;;; Code
;;;


(define Invalid-Code
  'Invalid-Code)


(define (invalid-code? obj)
  (eq? obj Invalid-Code))


(define (throw-invalid-code)
  (raise Invalid-Code))


;;;
;;;; Version
;;;


(define Invalid-Version
  'Invalid-Version)


(define (invalid-version? obj)
  (eq? obj Invalid-Version))


(define (throw-invalid-version)
  (raise Invalid-Version))


;;;
;;;; Exception
;;;


(define (connection-exception? exc)
  (or (os-exception? exc)
      (connection-broke-exception? exc)))


;;;
;;;; Protocol
;;;


(define *presence-code*
  (code-string->32-bit-integer "JAZZ"))

(define *presence-version*
  (new-version 1 0 0))


(define (write-port port binary? code version info)
  (cond (binary?
         (write-header code version port)
         (write-data info port))
        (else
         (write-text-header code version port)
         (write-text-data info port)))
  (force-output port))


(define (read-port port binary? expected-code expected-version)
  (receive (code version) (if binary? (read-header port) (read-text-header port))
    (validate-code expected-code code)
    (validate-version expected-version version)
    (if binary? (read-data port) (read-text-data port))))


;;;
;;;; Binary
;;;


(define (write-header code version port)
  (32-bit-integer->bytes code
    (lambda (b1 b2 b3 b4)
      (version->bytes version
        (lambda (b5 b6 b7 b8)
          (write-u8 b1 port)
          (write-u8 b2 port)
          (write-u8 b3 port)
          (write-u8 b4 port)
          (write-u8 b5 port)
          (write-u8 b6 port)
          (write-u8 b7 port)
          (write-u8 b8 port)
          (force-output port))))))


(define (read-header port)
  (let* ((b1 (read-u8 port))
         (b2 (read-u8 port))
         (b3 (read-u8 port))
         (b4 (read-u8 port))
         (b5 (read-u8 port))
         (b6 (read-u8 port))
         (b7 (read-u8 port))
         (b8 (read-u8 port)))
    (if (eof-object? b8)
        (throw-connection-broke "Read header received eof")
      (let ((code (bytes->32-bit-integer b1 b2 b3 b4))
            (version (bytes->version b5 b6 b7 b8)))
        (values code version)))))


(define (write-data data port)
  (write-binary data port))


(define (read-data port)
  (let ((data (read-binary port)))
    (if (eof-object? data)
        (throw-connection-broke "Read data received eof")
      data)))


;;;
;;;; Text
;;;


(define (write-text-header code version port)
  (32-bit-integer->bytes code
    (lambda (b1 b2 b3 b4)
      (version->bytes version
        (lambda (b5 b6 b7 b8)
          (write (list b1 b2 b3 b4 b5 b6 b7 b8) port)
          (force-output port))))))


(define (read-text-header port)
  (let ((data (read port)))
    (if (eof-object? data)
        (throw-connection-broke "Read header received eof")
      (bind (b1 b2 b3 b4 b5 b6 b7 b8) data
        (let ((code (bytes->32-bit-integer b1 b2 b3 b4))
              (version (bytes->version b5 b6 b7 b8)))
          (values code version))))))


(define (write-text-data data port)
  (write-text data port))


(define (read-text-data port)
  (let ((data (read-text port)))
    (if (eof-object? data)
        (throw-connection-broke "Read data received eof")
      data)))


(define (write-text data port)
  (define (encode obj)
    (cond ((null? obj)
           obj)
          ((pair? obj)
           (cons (encode (car obj))
                 (encode (cdr obj))))
          (else
           (let ((expr (serialize obj)))
             (if (serialized? expr)
                 (vector 'serialized
                         (serialized-class expr)
                         (serialized-content expr))
               expr)))))
  
  (write (encode data) port)
  (force-output port))


(define (read-text port)
  (define (decode obj)
    (cond ((null? obj)
           obj)
          ((pair? obj)
           (cons (decode (car obj))
                 (decode (cdr obj))))
          ((and (vector? obj)
                (> (vector-length obj) 0)
                (eq? (vector-ref obj 0) 'serialized))
           (deserialize
             (make-serialized (vector-ref obj 1)
                              (vector-ref obj 2))))
          (else
           obj)))
  
  (decode (read port)))


;;;
;;;; Validation
;;;


(define (validate-code expected-code code)
  (if (not (= code expected-code))
      (throw-invalid-code)))


(define (validate-version expected-version version)
  (if (not (version=? version expected-version))
      (throw-invalid-version)))
