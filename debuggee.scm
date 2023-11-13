;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Debuggee
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
;;;; Local
;;;


(define local-process
  #f)


(define (get-local-process)
  local-process)


(define (setup-local-process)
  (if (not local-process)
      (begin
        (set! local-process (new-debuggee-process-local-proxy (require-presence 'debugging) (new-debuggee-process)))
        (register-proxy-register-object (local-register 'debugging) 'debuggee local-process))))


;;;
;;;; Debugged Continuations
;;;


(define debugged-continuations-thread-group
  (make-thread-group 'debugged-continuations))


;;;
;;;; Controller
;;;


(define controller-debugger
  #f)

(define controller-debugger-cookie
  #f)


(define (get-controller-debugger)
  controller-debugger)


(define (attach-to-controller debugger-proxy focus?)
  (detach-from-controller)
  (set! controller-debugger debugger-proxy)
  (setup-local-process)
  (console-port-getter-set! console-port-getter)
  (console-clear-set! console-clear)
  (debugger-proxy-attach-process controller-debugger local-process focus?)
  ;(set-exception-debugger jazz-exception-debugger)
  ;(set-exception-hook exception-debugger-hook)
  (add-exit-job! detach-from-controller))


(define (ready-to-controller #!optional (debugger-cookie #f))
  (set! controller-debugger-cookie debugger-cookie)
  (debugger-proxy-ready-process controller-debugger local-process debugger-cookie))


(define (detach-from-controller)
  (when controller-debugger
    (detach-from-debugger)
    #; ;; register-object-error-when-reattaching
    (set! local-process #f)))


(define (detach-from-debugger)
  (when controller-debugger
    (debugger-proxy-detach-process controller-debugger local-process)
    (when (remote-proxy? controller-debugger)
      (closing-remote-proxy controller-debugger))
    (set! controller-debugger #f)
    (detach-consoles)))


(define (setup-debuggee #!key (debugger #f) (connection-exception-exit? #t))
  (let ((debugger-arg (or debugger (command-argument "debugger")))
        (interactive-arg (command-argument "debugger-interactive")))
    (when debugger-arg
      (receive (host port alternate-port) (parse-host/service debugger-arg)
        (if (not port)
            (error "Ill-formed debugger argument: {a}" debugger-arg)
          (call-with-catch connection-exception?
             (lambda (exc)
               (if (not connection-exception-exit?)
                   exc
                 (begin
                   (system-message "Unable to connect to debugger"
                                   title: "Application"
                                   type: 'problem)
                   (exit 1))))
            (lambda ()
              (attach-debuggee host port focus?: #t)
              #f)))))))


(define (attach-debuggee host port #!key (focus? #f))
  ;; wiqq (load-debuggee-units)
  (attach-debuggee-to-controller (connect-remote-reference (or host localhost) port 'debugger purpose: 'debugging) focus?: focus?))


(define (attach-debuggee-to-controller debugger-proxy #!key (focus? #f))
  (attach-to-controller debugger-proxy focus?))


(define (ready-debuggee)
  (let ((cookie-arg (command-argument "debugger-cookie")))
    (let ((cookie (and cookie-arg (parse-integer cookie-arg))))
      (when controller-debugger
        (ready-to-controller cookie)))))


(define (update-debuggee-arguments)
  (let ((title-arg (command-argument "process-title"))
        (traits-arg (command-argument "process-traits"))
        (icon-arg (command-argument "process-icon")))
    (when title-arg
      (current-process-title-set! title-arg))
    (when traits-arg
      (current-process-traits-set! traits-arg))
    (when icon-arg
      (current-process-icon-set! icon-arg))))


(define (update-debuggee-process)
  (when controller-debugger
    (update-process controller-debugger local-process)))


;;;
;;;; Process
;;;


(define (process-hash process)
  ;; fixme
  0)


;;;
;;;; Stops
;;;


(define *stops*
  '())


(define with-stops-mutex
  (let ((mutex (make-mutex 'stops)))
    (lambda (thunk)
      (mutex-lock! mutex)
      (thunk)
      (mutex-unlock! mutex))))


(define (stop-register stop-info)
  (with-stops-mutex
    (lambda ()
      (set! *stops* (append *stops* (list stop-info))))))


(define (stop-unregister stop-info)
  (with-stops-mutex
    (lambda ()
      (set! *stops* (remove stop-info *stops*)))))


(define (thread-active-stops thread)
  (let ((stops '()))
    (with-stops-mutex
      (lambda ()
        (for-each (lambda (stop-info)
                    (bind (stop thread-proxy stop-proxy) stop-info
                      (when (eq? (debuggee-stop-thread stop) thread)
                        (set! stops (append stops (list stop-proxy))))))
                  *stops*)))
    stops))


(define (post-stop exc)
  (continuation-capture
    (lambda (cont)
      (thread-write
        (primordial-thread)
        (lambda ()
          (when (local-debugger?)
            (jazz-handle-exception exc cont)))))))


(define (post-continuation cont)
  (let ((exc (new Error message: "Posted continuation")))
    (thread-write
      (primordial-thread)
      (lambda ()
        (jazz-handle-exception exc cont)))))


;;;
;;;; Loop
;;;


(define-type-of-object loop
  level)


(define (new-loop level)
  (let ((loop (make-loop
                'loop
                #f
                level)))
    (setup-object loop)
    loop))


(define current-loop
  (make-parameter #f))


(define (with-current-loop thunk)
  (let ((current (current-loop)))
    (let ((loop (new-loop (if current (+ (get-level current) 1) 0))))
      (parameterize ((current-loop loop))
        (thunk)))))


;;;
;;;; Console
;;;


(define-type-of-object console
  ;; gc protect
  thread-proxy
  remote
  pump
  tail
  head
  readtable
  repl-thread
  context
  history)


(define (new-console thread-proxy remote pump tail head)
  (let ((console (make-console
                   'console
                   #f
                   thread-proxy
                   remote
                   pump
                   tail
                   head
                   #f
                   #f
                   (unspecified)
                   #f)))
    (setup-object console)
    console))


(define *consoles*
  (make-table test: eq?))


(define with-consoles-mutex
  (let ((mutex (make-mutex 'consoles)))
    (lambda (thunk)
      (mutex-lock! mutex)
      (let ((result (thunk)))
        (mutex-unlock! mutex)
        result))))


(define (thread-console thread #!key (debugger #f) (readtable #f) (select? #f))
  (let ((debugger (or debugger controller-debugger)))
    (if (not debugger)
        #f
      (with-consoles-mutex
        (lambda ()
          (or (table-ref *consoles* thread #f)
              (let ((console (make-debuggee-console (new-debuggee-thread-local-proxy (require-presence 'debugging) (new-debuggee-thread thread)) debugger readtable select?)))
                (table-set! *consoles* thread console)
                console)))))))


(define (thread-console-pump-port thread)
  (console-tail (thread-console thread)))

(define (thread-console-port thread)
  (console-head (thread-console thread)))


(define (current-console)
  (thread-console (current-thread)))

(define current-console-context
  (make-parameter #f))


(define (make-debuggee-console thread-proxy debugger readtable select?)
  (let ((readtable-parameters (if readtable (list readtable: readtable) '())))
    (receive (head tail) (open-string-pipe `(permanent-close: #f ,@readtable-parameters))
      (let ((console (debugger-proxy-register-console debugger local-process thread-proxy select?)))
        (let ((pump (start-debuggee-console-pump debugger console tail)))
          (new-console thread-proxy console pump tail head))))))


(define (select-debugger-console)
  (debugger-proxy-select-console controller-debugger (console-remote (thread-console (current-thread) select?: #t))))


(define (persist-debugger-console)
  (debugger-proxy-persist-console controller-debugger (console-remote (thread-console (current-thread)))))


(define (clear-debugger-console)
  (debugger-proxy-clear-console controller-debugger (console-remote (thread-console (current-thread)))))


(define (close-console)
  (close-thread-console (current-thread)))


(define (close-thread-console thread)
  (let ((console (thread-console thread)))
    (when console
      (stop-repl-thread console)
      (detach-console thread console)
      (debugger-proxy-unregister-console controller-debugger (console-remote console)))))


(define (console-port-getter)
  (if (not controller-debugger)
      (terminal-port)
    (console-head (current-console))))


(define (detach-consoles)
  (for-each (lambda (pair)
              (bind (thread . console) pair
                (detach-console thread console)))
            (table-keys/values *consoles*)))


(define (detach-console thread console)
  (with-consoles-mutex
    (lambda ()
      (stop-debuggee-console-pump (console-pump console))
      (close-port (console-tail console))
      (close-port (console-head console))
      (table-clear *consoles* thread))))


(define (console-clear)
  (if (not controller-debugger)
      (clear-terminal)
    (clear-debugger-console)))


;;;
;;;; Pump
;;;


(define (start-debuggee-console-pump debugger console port)
  (let ((thread
          (make-thread
            (lambda ()
              (start-pump port
                (lambda (str)
                  (when debugger
                    (debugger-proxy-console-output debugger console str)))))
            'debuggee-pump)))
    (thread-base-priority-set! thread debugging-priority)
    (thread-start! thread)))


(define (stop-debuggee-console-pump thread)
  (exit-thread thread))


;;;
;;;; Exception
;;;


(define (jazz-exception-debugger exc)
  (declare (proper-tail-calls))
  (let ((cont (get-exception-context exc)))
    (if cont
        (jazz-handle-exception exc cont)
      (continuation-capture
        (lambda (cont)
          (jazz-handle-exception exc cont))))))


(define (with-jazz-exception-debugger thunk)
  (with-exception-debugger jazz-exception-debugger
    thunk))


(define (jazz-handle-exception exc cont)
  (let ((debugger (get-controller-debugger))
        (use (use-debugger?)))
    (if (or (not debugger) (not use))
        (invoke-exception-hook system-exception-hook exc)
      (with-system-exception-debugger
        (lambda ()
          (when (eq? use 'once)
            (use-debugger? #f))
          (let ((reason (exception-reason exc))
                (detail (exception-detail exc)))
            (invoke-debugger 'exception reason detail exc cont)))))))


(define (jazz-debugger?)
  (and (eq? (active-exception-debugger) jazz-exception-debugger)
       controller-debugger))


(define (local-debugger?)
  (and (jazz-debugger?)
       (local-proxy? controller-debugger)))


(define (without-local-debugger thunk)
  (if (local-debugger?)
      (with-system-exception-debugger
        thunk)
    (thunk)))


(define (using-debugger?)
  (command-argument "debugger"))


(define (break #!key (reason #f) (detail #f) (exception #f))
  (declare (proper-tail-calls))
  (continuation-capture
    (lambda (continuation)
      (invoke-debugger 'break reason detail exception continuation))))


(define (start-repl #!key (reason #f) (readtable #f) (select? #t))
  (declare (proper-tail-calls))
  (continuation-capture
    (lambda (continuation)
      (when readtable
        (thread-console (current-thread) readtable: readtable select?: select?))
      (when select?
        (select-debugger-console))
      (invoke-repl reason #f continuation))))


;;;
;;;; Repl
;;;


(define current-repl-level
  (make-parameter 0))

(define current-repl-frame
  (make-parameter #f))


(define (with-repl-thread thread reason port level thunk #!key (step? #f))
  (let ((previous-level (current-repl-level)))
    (parameterize ((current-repl-level level))
      (let ((repl-thread (start-repl-thread thread reason port level: level step?: step?)))
        (let ((console (thread-console thread)))
          (dynamic-wind
            (lambda ()
              #f)
            (lambda ()
              (thunk))
            (lambda ()
              (when console
                (stop-repl-thread console)))))))))


(define (start-repl-thread thread reason port #!key (level 0) (step? #f))
  (let ((repl-thread
          (make-thread
            (lambda ()
              (when (= level 0)
                (display-banner port))
              (when reason
                (format port "\2$\3")
                (display reason port)
                (newline port))
              (when (and (> level 0) (not step?))
                (display-prompt port level))
              (read-eval-print-loop thread port level))
            (if (= level 0)
                'debuggee-repl
              (string->symbol (format "repl{a}" level))))))
    (thread-base-priority-set! repl-thread debugging-priority)
    (thread-start! repl-thread)
    ;; robust try
    (let ((console (thread-console thread)))
      (when console
        (console-repl-thread-set! console repl-thread)))
    repl-thread))


(define (stop-repl-thread console)
  (let ((repl-thread (console-repl-thread console)))
    (when repl-thread
      (exit-thread repl-thread)
      (console-repl-thread-set! console #f))))


(define (display-banner port)
  (let ((title (current-process-title))
        (version (current-process-version)))
    (if (not version)
        (format port "\2banner {a}{%}{%}\3" title)
      (format port "\2banner {a} v{a}{%}{%}\3" title version)))
  (format port "\2banner-prompt > \3")
  (force-output port))


(define (display-prompt port level)
  (define (prompt)
    (if (> level 0)
        (format "{a}> " level)
      "> "))
  
  (format port "\2$\3")
  (format port "\2prompt {a}\3" (prompt))
  (force-output port))


;;;
;;;; Loop
;;;


(define (read-eval-print-loop thread port level)
  (define (read-eval-print thread resume port level)
    (define (console-read)
      (let ((console (thread-console thread)))
        (with-readtable (or (and console (console-readtable console)) scheme-readtable)
          (lambda ()
            (catch-exception-filter
              (lambda (exc)
                #t)
              (lambda (exc)
                (thread-post thread 'exception
                  (lambda ()
                    (throw exc)))
                (continuation-return resume #f))
              (lambda ()
                (read port)))))))
    
    (define (console-eval expr)
      (define (parse-unquote-command expr)
        (if (and (pair? expr)
                 (eq? (car expr) 'unquote)
                 (pair? (cdr expr)))
            (let ((unquoted (cadr expr)))
              (cond ((symbol? unquoted)
                     (values unquoted #f))
                    ((pair? unquoted)
                     (values (car unquoted) (cdr unquoted)))
                    (else
                     (values #f #f))))
          (values #f #f)))
      
      (define (current-frame)
        (let ((frame-box (current-repl-frame)))
          (if frame-box
              (unbox frame-box)
            #f)))
      
      (define (frame-evaluator frame)
        (if frame
            (let ((cont (serial->object (get-continuation frame))))
              (lambda (runner expr)
                (eval-within-no-winding runner expr cont)))
          (lambda (runner expr)
            (runner
              (lambda ()
                (eval expr))))))
      
      (define (eval-in-context context frame evaluator expr)
        (define (local-names variables)
          (map (lambda (var)
                 (string->symbol (second (car var))))
               variables))
        
        (let ((local-variables (if frame (get-variables frame :lexical) '())))
          (let ((local-names (local-names local-variables)))
            (let ((effective-context context))
              (evaluator (lambda (thunk)
                           (thunk))
                         expr)))))
      
      (let ((console (current-console)))
        (let ((context (determine-console-context console)))
          (receive (cmd arguments) (parse-unquote-command expr)
            (define (context-eval context expr)
              (let ((frame (current-frame)))
                (let ((evaluator (frame-evaluator frame)))
                  (eval-in-context context frame evaluator expr))))
            
            (if cmd
                (let ((command (registered-console-command cmd)))
                  (command cmd arguments console console-read console-eval console-print context-eval))
              (context-eval context expr))))))
    
    (define (console-print result)
      (when (and (thread-call-result? result)
                 (specified? result))
        (repl-result-history-add result)
        (let ((values (call-with-values (lambda () result) list)))
          (for-each (lambda (value)
                      (format port "{s}" value)
                      (format port "{%}"))
                    values)))
      (display-prompt port level)
      (force-output port))
    
    (let ((expr (console-read)))
      (if (eof-object? expr)
          (thread-post thread 'resume-loop
            (lambda ()
              (let ((restarts (find-restarts 'resume-loop)))
                (when (> (length restarts) 1)
                  (newline port)
                  ;; skip the current resume-loop restart
                  (let ((restart (second restarts)))
                    (invoke-restart restart))))))
        (let ((result
                (thread-call thread 'console-eval
                  (lambda ()
                    (console-eval expr)))))
          (console-print result)))))
  
  (declare (proper-tail-calls))
  (let iterate ()
    (continuation-capture
      (lambda (resume)
        (read-eval-print thread resume port level)))
    (iterate)))


;;;
;;;; Context
;;;


(define default-context
  ':not-set)

(define (get-default-context)
  default-context)

(define (set-default-context ctx)
  (set! default-context ctx))


(define (context-alias context)
  (and (enumerator? context)
       (if (and (eq? context ':default)
                (not (eq? default-context ':not-set)))
           default-context
         #f)))


(define (determine-console-context console)
  (define (context-init console)
    (let ((context (console-context console)))
      (if (unspecified? context)
          (let ((new-context ':default))
            (console-context-set! console new-context)
            new-context)
        context)))
  
  (if (not console)
      #f
    (context-init console)))


;;;
;;;; Commands
;;;


(define *console-commands*
  (make-table test: eq?))


(define (register-console-command cmd proc)
  (table-set! *console-commands* cmd proc))


(define (registered-console-command cmd)
  (or (table-ref *console-commands* cmd #f)
      (error "Unknown console command: {s}" cmd)))


(define (in-command cmd arguments console read eval print context-eval)
  (if (or (not arguments) (null? arguments))
      (determine-console-context console)
    (let ((ctx (car arguments)))
      (let ((new-context (or (context-alias ctx) (eval ctx))))
        (when (symbol? new-context)
          (load-unit new-context))
        (if (not-null? (cdr arguments))
            (let ((expr (cadr arguments)))
              (context-eval new-context expr))
          (begin
            (when console
              (set-context console new-context)
              (set-readtable console (if (not new-context) #f jazz-readtable)))
            new-context))))))


(register-console-command 'in in-command)


(define (scheme-command cmd arguments console read eval print context-eval)
  (let ((expr (car arguments)))
    (context-eval #f expr)))


(register-console-command 'scheme scheme-command)


;;;
;;;; Debugger
;;;


(define (invoke-repl reason detail continuation)
  (let ((thread (current-thread))
        (port (current-console-port))
        (level (if (not (current-loop)) 0 (+ (current-repl-level) 1))))
    (with-repl-thread thread reason port level
      (lambda ()
        (parameterize ((current-repl-frame (box #f)))
          (with-current-loop
            (lambda ()
              (debuggee-loop))))))))


(define (invoke-debugger kind reason detail exc continuation #!key (locat #f) (stepper #f))
  (define (compute-restarts thread)
    (map (lambda (restart)
           (new Debuggee-Restart-Local-Proxy (require-presence 'debugging) (new Jazz-Debuggee-Restart thread restart)))
         (current-restarts)))
  
  (let ((thread (current-thread))
        (port (current-console-port))
        (level (current-repl-level)))
    (with-repl-thread thread reason port (+ level 1)
      (lambda ()
        (parameterize ((current-repl-frame (box #f)))
          (let ((stop (new-debuggee-stop thread kind reason detail exc continuation (compute-restarts thread) locat stepper)))
            (let ((thread-proxy (new-debuggee-thread-local-proxy (require-presence 'debugging) (new-debuggee-thread thread)))
                  (stop-proxy (new-debuggee-stop-local-proxy (require-presence 'debugging) stop)))
              (let ((stop-info (list stop thread-proxy stop-proxy)))
                (dynamic-wind
                  (lambda ()
                    (stop-register stop-info)
                    (debugger-proxy-register-stop controller-debugger local-process thread-proxy stop-proxy step?: stepper))
                  (lambda ()
                    (with-current-loop
                      (lambda ()
                        (debuggee-loop))))
                  (lambda ()
                    (stop-unregister stop-info)
                    (when controller-debugger
                      (debugger-proxy-unregister-stop controller-debugger local-process thread-proxy stop-proxy)))))))))
      step?: stepper)))


(define (debuggee-loop)
  (declare (proper-tail-calls))
  (let restart-loop ()
    (with-restart-catcher 'resume-loop (present-current-loop-restart)
      (lambda ()
        (let loop ()
          (with-jazz-exception-debugger
            (lambda ()
              (thread-process)))
          (loop))))
    (restart-loop)))


(define (present-current-loop-restart)
  (let ((loop (current-loop)))
    (and loop
         (let ((level (loop-level loop)))
           (format "Resume loop{a}"
                   (if (= level 0)
                       ""
                     (format " {a}" level)))))))
