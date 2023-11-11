;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Version
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


#;
(method meta override (marshall-object version object)
  (serialize-object (class-of object)
                    (vector (serialize (get-major object))
                            (serialize (get-minor object))
                            (serialize (get-revision object))
                            (serialize (get-build object))
                            (serialize (get-stage object)))))


#;
(method meta override (unmarshall-object version content)
  (bind-vector (major minor revision build stage) content
    (allocate version
              (deserialize major)
              (deserialize minor)
              (deserialize revision)
              (deserialize build)
              (deserialize stage))))


(define-type version
  major
  minor
  revision
  build
  stage)


(define (new-version major minor #!optional (revision 0) (build 0) (stage #f))
  (make-version
    major
    minor
    revision
    build
    stage))


(define (version=? x y)
  (and (= (version-major x) (version-major y))
       (= (version-minor x) (version-minor y))
       (= (version-revision x) (version-revision y))
       (= (version-build x) (version-build y))))


(define (version-print version output readably)
  (format output "~{{a} {a}}"
    (category-name (class-of version))
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
