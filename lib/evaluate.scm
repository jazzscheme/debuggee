;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Evaluation
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


(define (evaluate-forms syntax str container line col evaluation-mode #!key (file #f) (readtable #f) (evaluate? #t) (scripting? #f))
  (define (toplevel-form? expr)
    (and (pair? (source-code expr))
         (memq (source-code (car (source-code expr))) '(unit module script))))
  
  (define (parse-toplevel expr)
    (let ((kind (source-code (car (source-code expr))))
          (expr (cdr (source-code expr))))
      (let ((first (source-code (car expr))))
        (if (memq first '(protected public))
            (values kind (source-code (cadr expr)) first)
          (values kind first 'public)))))
  
  (define (present result)
    (when (specified? result)
      (let ((values (call-with-values (lambda () result) list)))
        (for-each (lambda (value)
                    (format ':console "\2$\3\2color Ochre {s}\3{%}" value))
                  values))
      (display-prompt (current-console-port) (current-repl-level))
      (force-output (current-console-port))))
  
  (let ((forms ;; hack around the worker giving me a -1 line that was
               ;; probably a hack around gambit's first line being 1!?
               (let ((line (max 0 line)))
                 ;; hack around what seems to be a change in the semantics
                 ;; of ##input-port-line-set! and ##input-port-column-set!
                 (let ((str (string-append (make-string line #\newline) (make-string col #\space) str))
                       (line 0)
                       (col 0))
                   (call-with-input-string str
                     (lambda (port)
                       (read-source-all port container line col)))))))
    (let ((first (car forms)))
      (if (and (eq? syntax 'jazz) (toplevel-form? first) (null? (cdr forms)))
          (let ((expression first))
            (receive (kind unit-name access) (parse-toplevel first)
              (if (not evaluate?)
                  (expand-form expression)
                (present (eval expression)))))
          (let ((expression (cons 'begin forms)))
            (present (eval expression)))))))
