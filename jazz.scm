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
;;;; Atom
;;;


(define (atom? value)
  (or (null? value)
      (boolean? value)
      (char? value)
      (string? value)
      (symbol? value)
      (keyword? value)
      (number? value)))


;;;
;;;; Boolean
;;;


(define (boolean obj)
  (if obj #t #f))


;;;
;;;; Serial
;;;


(define (object->serial obj)
  (object->serial-number obj))


(define (serial->object number)
  (serial-number->object number))


;;;
;;;; Message
;;;


(define (system-message text . rest)
  (let ((port (console-port)))
    (display text port)
    (newline port)))


;;;
;;;; Number
;;;


(define (naturals from to)
  (let iter ((n (fx- to 1)) (lst '()))
       (if (fx< n from)
           lst
         (iter (fx- n 1) (cons n lst)))))


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


(define (find-if predicate lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (null? scan)
        #f
      (let ((value (car scan)))
        (if (predicate value)
            value
          (iter (cdr scan)))))))


(define (collect-if predicate lst)
  (declare (proper-tail-calls))
  (let iter ((scan lst))
    (if (not (null? scan))
        (let ((value (car scan)))
          (if (predicate value)
              (cons value (iter (cdr scan)))
            (iter (cdr scan))))
      '())))


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
;;;; Table
;;;


(define (table-clear table key)
  (table-set! table key))


(define (table-keys/values table)
  (let ((list '()))
    (iterate-table table
      (lambda (key value)
        (set! list (cons (cons key value) list))))
    list))


;; safe versions of table-for-each and table-search


(define (iterate-table table proc)
  (for-each (lambda (pair)
              (proc (car pair) (cdr pair)))
            (table->list table)))


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
;;;; Time
;;;


(define current-seconds
  ##current-time-point)


(define systime->seconds
  time->seconds)

(define seconds->systime
  seconds->time)


;;;
;;;; Timeout
;;;


;; copied from _repl
(define (write-timeout to moment port)
  (write-string " " port)
  (let* ((expiry (fl- to moment))
         (e (fl/ (flround (fl* 10.0 expiry)) 10.0)))
    (write (if (integer? e) (inexact->exact e) e) port))
  (write-string "s" port))


;;;
;;;; Output
;;;


(define *console-clear*
  #f)

(define (console-clear-set! clear)
  (set! *console-clear* clear))


(define (terminal-port)
  (console-port))


;;;
;;;; Printers
;;;


(define *printers*
  (make-table test: eq?))


(define (register-printer name proc)
  (table-set! *printers* name proc))


(define (with-printer printer proc)
  (if (eq? printer ':string)
      (let ((output (open-output-string)))
        (proc output)
        (get-output-string output))
    (proc
      (let ((printer-proc (table-ref *printers* printer #f)))
        (if printer-proc
            (printer-proc)
          printer)))))


(register-printer ':terminal terminal-port)


;;;
;;;; Console
;;;


(define *console-port-getter*
  #f)


(define (console-port-getter-set! getter)
  (set! *console-port-getter* getter))


(define (current-console-port)
  (if (not *console-port-getter*)
      (terminal-port)
    (*console-port-getter*)))


(define (attached-console-port)
  (if (not *console-port-getter*)
      (and (terminal-available?) (terminal-port))
    (*console-port-getter*)))


(define (console-input-port)
  (current-console-port))

(define (console-output-port)
  (current-console-port))


(define (force-console)
  (force-output (current-console-port)))


(register-printer ':console current-console-port)


;;;
;;;; Restart
;;;


(define-type-of-object restart
  name
  message
  handler)


(define (new-restart name message handler)
  (let ((restart
          (make-restart
            'restart
            #f
            name
            message
            handler)))
    (setup-object restart)
    restart))


(define current-restarts
  (make-parameter '()))


(define (with-restart-handler name message handler thunk)
  (parameterize ((current-restarts (cons (new-restart name message handler) (current-restarts))))
    (thunk)))


(define (with-restart-catcher name message thunk)
  (continuation-capture
    (lambda (catcher-cont)
      (with-restart-handler name message
        (lambda rest
          (continuation-return catcher-cont
            (if (not (null? rest))
                (car rest)
              #f)))
        thunk))))


(define (find-restart name)
  (find-if (lambda (restart)
             (eq? (restart-name restart) name))
           (current-restarts)))


(define (find-restarts name)
  (collect-if (lambda (restart)
                (eq? (restart-name restart) name))
              (current-restarts)))


(define (invoke-restart restart . rest)
  (let ((handler (restart-handler restart)))
    (apply handler rest)))


;;;
;;;; Thread
;;;


(define primordial-thread-object
  (current-thread))


(define (primordial-thread)
  primordial-thread-object)


(define (primordial-thread?)
  (eq? (current-thread) (primordial-thread)))


(define pristine-thread-continuation
  (thread-join!
    (thread-start!
      (make-thread
        (lambda ()
          (continuation-capture
            (lambda (cont)
              cont)))))))


(define (thread-exit)
  (continuation-return pristine-thread-continuation #f))


(define (exit-thread thread)
  (thread-int! thread thread-exit))


(define (thread-group-all-threads thread-group)
  (apply append (thread-group->thread-list thread-group)
    (map thread-group-all-threads (thread-group->thread-group-list thread-group))))


(define (top-threads)
  (thread-group-all-threads (thread-thread-group (primordial-thread))))


(define (present-thread-state state)
  (let ((port (open-output-string))
        (moment (current-seconds)))
    (cond ((thread-state-uninitialized? state)
           (system-format port "Uninitialized"))
          ((thread-state-initialized? state)
           (system-format port "Initialized"))
          ((thread-state-normally-terminated? state)
           (system-format port "Normally terminated"))
          ((thread-state-abnormally-terminated? state)
           (system-format port "Abnormally terminated"))
          ((thread-state-running? state)
           (system-format port "Running"))
          ((thread-state-waiting? state)
           (let ((wf (thread-state-waiting-for state))
                 (to (thread-state-waiting-timeout state)))
             (cond (wf
                    (system-format port "Waiting ")
                    (write wf port)
                    (if (mutex? wf)
                        (begin
                          (write-string " " port)
                          (write (mutex-state wf) port)))
                    (if to
                        (write-timeout (systime->seconds to) moment port)))
                   (to
                    (system-format port "Sleeping")
                    (write-timeout (systime->seconds to) moment port)))))
          (else
           (write state port)))
    
    (get-output-string port)))


(define (safe-present-object object #!optional (max-width #f))
  (with-exception-catcher
    (lambda (exc)
      ;; add the exception reason somehow!?
      (let ((class-string
              (with-exception-catcher
                (lambda (exc)
                  "")
                (lambda ()
                  (string-append " " (symbol->string (if (object? object) (object-class object) '<unknown>)))))))
        (string-append "#<unprintable" class-string " #" (number->string (object->serial object)) ">")))
    (lambda ()
      (if max-width
          (object->string object max-width)
        (object->string object)))))


(define thread-queues
  (make-table test: eq? weak-keys: #t))

(define thread-queues-mutex
  (make-mutex 'thread-queues))


(define (thread-has-port? thread)
  (table-ref thread-queues thread #f))


(define (get-thread-queue thread)
  (mutex-lock! thread-queues-mutex)
  (let ((queue (or (table-ref thread-queues thread #f)
                   (let ((queue (open-vector)))
                     (table-set! thread-queues thread queue)
                     queue))))
    (mutex-unlock! thread-queues-mutex)
    queue))


(define (thread-write-message thread thunk)
  (let ((thread-queue (get-thread-queue thread)))
    (write thunk thread-queue)
    (force-output thread-queue)))


(define (thread-read-message #!key (timeout +inf.0))
  (let ((thread-queue (get-thread-queue (current-thread))))
    (input-port-timeout-set! thread-queue timeout)
    (read thread-queue)))


(define (thread-write thread thunk)
  (thread-write-message thread thunk))


(define (thread-read #!key (timeout +inf.0))
  (thread-read-message timeout: timeout))


(define (thread-process #!key (timeout +inf.0))
  (let ((thunk (thread-read-message timeout: timeout)))
    (when (not (eof-object? thunk))
      (thunk))))


(define (thread-post thread name thunk)
  ;; posts are never synchronous
  (thread-write thread thunk))


(define thread-call-noresult
  (list 'thread-call-noresult))


(define (thread-call-result? result)
  (not (eq? result thread-call-noresult)))


(define (thread-call thread name thunk)
  (if (eq? thread (current-thread))
      (thunk)
    (let ((mutex (make-mutex name)))
      (mutex-lock! mutex)
      (mutex-specific-set! mutex thread-call-noresult)
      (thread-write thread
        (lambda ()
          (dynamic-wind
            (lambda ()
              #f)
            (lambda ()
              (mutex-specific-set! mutex (thunk)))
            (lambda ()
              (mutex-unlock! mutex)))))
      (mutex-lock! mutex)
      (mutex-unlock! mutex)
      (mutex-specific mutex))))


;;;
;;;; Stack
;;;


(define procedure-name-cache
  (make-table test: eq?))


(define (get-procedure-name procedure)
  (if procedure
      (or (table-ref procedure-name-cache procedure #f)
          (let ((name (procedure-name procedure)))
            (table-set! procedure-name-cache procedure name)
            name))
    "(interaction)"))


(define (get-continuation-name cont)
  (get-procedure-name (##continuation-creator cont)))


(define (continuation-stack cont #!key (dynamic-environment? #f) (lexical-environment? #f) (identifiers? #f) (locations? #f) (depth #f))
  (define (package-name name)
    (if (and identifiers? (symbol? name))
        (reference-name name)
      name))
  
  (define (package-variable variable)
    (bind (name value mutable?) variable
      (list name value)))
  
  (define (package-variables variables)
    (map package-variable variables))
  
  (let ((stack (get-continuation-stack cont depth)))
    (map (lambda (cont)
           (let ((name (package-name (get-continuation-name cont))))
             (cond ((and (not dynamic-environment?) (not lexical-environment?))
                    (if (not locations?)
                        name
                      (cons name
                            (let ((location (get-continuation-location cont)))
                              (if (not location)
                                  (list #f #f)
                                (bind (container line col) location
                                  (list line col)))))))
                   ((not dynamic-environment?)
                    (cons name (package-variables (get-continuation-lexical-environment cont))))
                   (else
                    (cons name (append (package-variables (get-continuation-dynamic-environment cont))
                                       (package-variables (get-continuation-lexical-environment cont))))))))
         stack)))


(define (execution-stack #!key (dynamic-environment? #f) (lexical-environment? #f) (identifiers? #f) (locations? #f) (depth #f))
  (continuation-capture
    (lambda (cont)
      (continuation-stack cont dynamic-environment?: dynamic-environment? lexical-environment?: lexical-environment? identifiers?: identifiers? locations?: locations? depth: depth))))


;;;
;;;; I/O
;;;


(define (start-pump port proc)
  (declare (proper-tail-calls))
  (let ((size 1000))
    (let ((buffer (make-string size)))
      (let iterate ()
        (let ((n (read-substring buffer 0 size port 1)))
          (proc (if (> n 0)
                    (substring buffer 0 n)
                  ;; eof
                  #f))
          (iterate))))))


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
  id: 16E8E4BA-FD59-4E7A-A219-DFE934B6CC18
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


;;;
;;;; Port
;;;


(define (write-string str port)
  (##write-string str port))


;;;
;;;; Format
;;;


(define (format . rest)
  (define (format-to output fmt-string arguments)
    (let ((control (open-input-string fmt-string))
          (done? #f))
      (define (format-directive)
        (let ((directive (read control)))
          (read-char control)
          (case directive
            ((a)
             (display (car arguments) output)
             (set! arguments (cdr arguments)))
            ((s)
             (write (car arguments) output)
             (set! arguments (cdr arguments)))
            ((t)
             (write (car arguments) output)
             (set! arguments (cdr arguments)))
            ((l)
             (let ((first? #t))
               (for-each (lambda (element)
                           (if first?
                               (set! first? #f)
                             (display " " output))
                           (display element output))
                         (car arguments)))
             (set! arguments (cdr arguments)))
            ((%)
             (newline output))
            (else
             (error "Unknown format directive" directive)))))
      
      (let iter ()
           (let ((c (read-char control)))
             (if (not (eof-object? c))
                 (begin
                   (cond ((eqv? c #\~)
                          (write-char (read-char control) output))
                         ((eqv? c #\{)
                          (format-directive))
                         (else
                          (write-char c output)))
                   (iter)))))))
  
  (define (parse-format proc)
    (if (string? (car rest))
        (proc ':string (car rest) (cdr rest))
      (proc (car rest) (cadr rest) (cddr rest))))
  
  (parse-format
    (lambda (destination fmt-string arguments)
      (with-printer destination
        (lambda (output)
          (format-to output fmt-string arguments))))))


(define system-format
  format)
