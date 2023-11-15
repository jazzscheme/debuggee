;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Sequences
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


(define-type-of-class sequence-class
  extender: define-type-of-sequence-class)


(define (sequence? obj)
  (boolean (sequence-class obj)))


(define (sequence-length seq)
  ((class-dispatch seq 'sequence-length) seq))


(define (sequence-ref seq n)
  ((class-dispatch seq 'sequence-ref) seq n))


(define (sequence-set! seq n val)
  ((class-dispatch seq 'sequence-set!) seq n val))


;;;
;;;; Vector
;;;


(define-type-of-sequence-class vector-class)


(define (new-vector-class)
  (let ((vector-class
          (make-vector-class
            'Vector
            #f)))
    (setup-class vector-class)
    (class-add-method vector-class 'sequence-length vector-class-sequence-length)
    (class-add-method vector-class 'sequence-ref vector-class-sequence-ref)
    (class-add-method vector-class 'sequence-set! vector-class-sequence-set!)
    vector-class))


(define (vector-class-sequence-length vec)
  (vector-length vec))


(define (vector-class-sequence-ref vec n)
  (vector-ref vec n))


(define (vector-class-sequence-set! vec n obj)
  (vector-set! vec n obj))


(define Vector
  (new-vector-class))


;;;
;;;; S8Vector
;;;


(define-type-of-sequence-class s8vector-class)


(define (new-s8vector-class)
  (let ((s8vector-class
          (make-s8vector-class
            'S8Vector
            #f)))
    (setup-class s8vector-class)
    (class-add-method s8vector-class 'sequence-length s8vector-class-sequence-length)
    (class-add-method s8vector-class 'sequence-ref s8vector-class-sequence-ref)
    (class-add-method s8vector-class 'sequence-set! s8vector-class-sequence-set!)
    s8vector-class))


(define (s8vector-class-sequence-length vec)
  (s8vector-length vec))


(define (s8vector-class-sequence-ref vec n)
  (s8vector-ref vec n))


(define (s8vector-class-sequence-set! vec n obj)
  (s8vector-set! vec n obj))


(define S8Vector
  (new-s8vector-class))


;;;
;;;; U8Vector
;;;


(define-type-of-sequence-class u8vector-class)


(define (new-u8vector-class)
  (let ((u8vector-class
          (make-u8vector-class
            'U8Vector
            #f)))
    (setup-class u8vector-class)
    (class-add-method u8vector-class 'sequence-length u8vector-class-sequence-length)
    (class-add-method u8vector-class 'sequence-ref u8vector-class-sequence-ref)
    (class-add-method u8vector-class 'sequence-set! u8vector-class-sequence-set!)
    u8vector-class))


(define (u8vector-class-sequence-length vec)
  (u8vector-length vec))


(define (u8vector-class-sequence-ref vec n)
  (u8vector-ref vec n))


(define (u8vector-class-sequence-set! vec n obj)
  (u8vector-set! vec n obj))


(define U8Vector
  (new-u8vector-class))


;;;
;;;; S16Vector
;;;


(define-type-of-sequence-class s16vector-class)


(define (new-s16vector-class)
  (let ((s16vector-class
          (make-s16vector-class
            'S16Vector
            #f)))
    (setup-class s16vector-class)
    (class-add-method s16vector-class 'sequence-length s16vector-class-sequence-length)
    (class-add-method s16vector-class 'sequence-ref s16vector-class-sequence-ref)
    (class-add-method s16vector-class 'sequence-set! s16vector-class-sequence-set!)
    s16vector-class))


(define (s16vector-class-sequence-length vec)
  (s16vector-length vec))


(define (s16vector-class-sequence-ref vec n)
  (s16vector-ref vec n))


(define (s16vector-class-sequence-set! vec n obj)
  (s16vector-set! vec n obj))


(define S16Vector
  (new-s16vector-class))


;;;
;;;; U16Vector
;;;


(define-type-of-sequence-class u16vector-class)


(define (new-u16vector-class)
  (let ((u16vector-class
          (make-u16vector-class
            'U16Vector
            #f)))
    (setup-class u16vector-class)
    (class-add-method u16vector-class 'sequence-length u16vector-class-sequence-length)
    (class-add-method u16vector-class 'sequence-ref u16vector-class-sequence-ref)
    (class-add-method u16vector-class 'sequence-set! u16vector-class-sequence-set!)
    u16vector-class))


(define (u16vector-class-sequence-length vec)
  (u16vector-length vec))


(define (u16vector-class-sequence-ref vec n)
  (u16vector-ref vec n))


(define (u16vector-class-sequence-set! vec n obj)
  (u16vector-set! vec n obj))


(define U16Vector
  (new-u16vector-class))


;;;
;;;; S32Vector
;;;


(define-type-of-sequence-class s32vector-class)


(define (new-s32vector-class)
  (let ((s32vector-class
          (make-s32vector-class
            'S32Vector
            #f)))
    (setup-class s32vector-class)
    (class-add-method s32vector-class 'sequence-length s32vector-class-sequence-length)
    (class-add-method s32vector-class 'sequence-ref s32vector-class-sequence-ref)
    (class-add-method s32vector-class 'sequence-set! s32vector-class-sequence-set!)
    s32vector-class))


(define (s32vector-class-sequence-length vec)
  (s32vector-length vec))


(define (s32vector-class-sequence-ref vec n)
  (s32vector-ref vec n))


(define (s32vector-class-sequence-set! vec n obj)
  (s32vector-set! vec n obj))


(define S32Vector
  (new-s32vector-class))


;;;
;;;; U32Vector
;;;


(define-type-of-sequence-class u32vector-class)


(define (new-u32vector-class)
  (let ((u32vector-class
          (make-u32vector-class
            'U32Vector
            #f)))
    (setup-class u32vector-class)
    (class-add-method u32vector-class 'sequence-length u32vector-class-sequence-length)
    (class-add-method u32vector-class 'sequence-ref u32vector-class-sequence-ref)
    (class-add-method u32vector-class 'sequence-set! u32vector-class-sequence-set!)
    u32vector-class))


(define (u32vector-class-sequence-length vec)
  (u32vector-length vec))


(define (u32vector-class-sequence-ref vec n)
  (u32vector-ref vec n))


(define (u32vector-class-sequence-set! vec n obj)
  (u32vector-set! vec n obj))


(define U32Vector
  (new-u32vector-class))


;;;
;;;; S64Vector
;;;


(define-type-of-sequence-class s64vector-class)


(define (new-s64vector-class)
  (let ((s64vector-class
          (make-s64vector-class
            'S64Vector
            #f)))
    (setup-class s64vector-class)
    (class-add-method s64vector-class 'sequence-length s64vector-class-sequence-length)
    (class-add-method s64vector-class 'sequence-ref s64vector-class-sequence-ref)
    (class-add-method s64vector-class 'sequence-set! s64vector-class-sequence-set!)
    s64vector-class))


(define (s64vector-class-sequence-length vec)
  (s64vector-length vec))


(define (s64vector-class-sequence-ref vec n)
  (s64vector-ref vec n))


(define (s64vector-class-sequence-set! vec n obj)
  (s64vector-set! vec n obj))


(define S64Vector
  (new-s64vector-class))


;;;
;;;; U64Vector
;;;


(define-type-of-sequence-class u64vector-class)


(define (new-u64vector-class)
  (let ((u64vector-class
          (make-u64vector-class
            'U64Vector
            #f)))
    (setup-class u64vector-class)
    (class-add-method u64vector-class 'sequence-length u64vector-class-sequence-length)
    (class-add-method u64vector-class 'sequence-ref u64vector-class-sequence-ref)
    (class-add-method u64vector-class 'sequence-set! u64vector-class-sequence-set!)
    u64vector-class))


(define (u64vector-class-sequence-length vec)
  (u64vector-length vec))


(define (u64vector-class-sequence-ref vec n)
  (u64vector-ref vec n))


(define (u64vector-class-sequence-set! vec n obj)
  (u64vector-set! vec n obj))


(define U64Vector
  (new-u64vector-class))


;;;
;;;; F32Vector
;;;


(define-type-of-sequence-class f32vector-class)


(define (new-f32vector-class)
  (let ((f32vector-class
          (make-f32vector-class
            'F32Vector
            #f)))
    (setup-class f32vector-class)
    (class-add-method f32vector-class 'sequence-length f32vector-class-sequence-length)
    (class-add-method f32vector-class 'sequence-ref f32vector-class-sequence-ref)
    (class-add-method f32vector-class 'sequence-set! f32vector-class-sequence-set!)
    f32vector-class))


(define (f32vector-class-sequence-length vec)
  (f32vector-length vec))


(define (f32vector-class-sequence-ref vec n)
  (f32vector-ref vec n))


(define (f32vector-class-sequence-set! vec n obj)
  (f32vector-set! vec n obj))


(define F32Vector
  (new-f32vector-class))


;;;
;;;; F64Vector
;;;


(define-type-of-sequence-class f64vector-class)


(define (new-f64vector-class)
  (let ((f64vector-class
          (make-f64vector-class
            'F64Vector
            #f)))
    (setup-class f64vector-class)
    (class-add-method f64vector-class 'sequence-length f64vector-class-sequence-length)
    (class-add-method f64vector-class 'sequence-ref f64vector-class-sequence-ref)
    (class-add-method f64vector-class 'sequence-set! f64vector-class-sequence-set!)
    f64vector-class))


(define (f64vector-class-sequence-length vec)
  (f64vector-length vec))


(define (f64vector-class-sequence-ref vec n)
  (f64vector-ref vec n))


(define (f64vector-class-sequence-set! vec n obj)
  (f64vector-set! vec n obj))


(define F64Vector
  (new-f64vector-class))
