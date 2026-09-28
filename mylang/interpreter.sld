(define-library (mylang interpreter)
  (export
   make-interp make-interp-raw interp? interp-stack interp-stack-set! interp-env interp-env-set! interp-token-line interp-token-line-set! interp-call-stack interp-call-stack-set! interp-file interp-file-set! interp-name interp-name-set!
   stack-push! stack-pop!
   current-frame
   interp-error!
   call-with-frame
   call-lambda!
   invoke!
   exec-block
   apply-callable!
   interp-run
   execute-body)
  
  (import
   (scheme base)
   (scheme write)
   (scheme process-context)
   (mylang values)
   (mylang tokens)
   (mylang env)
   (mylang lexer)
   (mylang parser)
   (mylang error))
  
  (begin
    (define-record-type <interp>
      (make-interp-raw stack env token-line file name call-stack) 
      interp?
      (stack interp-stack interp-stack-set!)
      (env interp-env interp-env-set!)
      (token-line interp-token-line interp-token-line-set!)
      (file interp-file interp-file-set!)
      (name interp-name interp-name-set!)
      (call-stack interp-call-stack interp-call-stack-set!))
    
    (define (stack-push! interp value)
      (interp-stack-set! interp (cons value (interp-stack interp))))
    
    (define (stack-pop! interp)
      (let ((s (interp-stack interp)))
        (if (null? s)
            (interp-error! interp "StackUnderflowError" "pop from empty stack")
            (begin (interp-stack-set! interp (cdr s))
                   (car s)))))

    (define (current-frame interp)
      (make-frame (interp-file interp)
                  (interp-token-line interp)
                  (interp-name interp)))
    
    (define (interp-error! interp error-name message);builtinはこいつを投げる
      (raise (make-mylang-error error-name message (cons (current-frame interp) (interp-call-stack interp)))));一番最初に起動した時だから、traceは空

    (define (call-with-frame interp file line name thunk)
      (let ((saved-file (interp-file interp))
            (saved-name (interp-name interp))
            (saved-line (interp-token-line interp))
            (saved-env (interp-env interp))
            (saved-call-stack (interp-call-stack interp))
            (caller-frame (current-frame interp)))
        (dynamic-wind
          (lambda ()
            (interp-file-set! interp file)
            (interp-token-line-set! interp line)
            (interp-name-set! interp name)
            (interp-call-stack-set! interp (cons caller-frame  saved-call-stack)))
          thunk;実行
          (lambda ();全部戻す（絶対）
            (interp-file-set! interp saved-file)
            (interp-token-line-set! interp saved-line)
            (interp-name-set! interp saved-name)
            (interp-env-set! interp saved-env)
            (interp-call-stack-set! interp saved-call-stack)))))

    (define (call-lambda! interp lmb)
      (call-with-frame
       interp
       (lambda-value-file lmb)
       (lambda-value-line lmb)
       (value->write-string lmb)
       (lambda ()
         (interp-env-set! interp (make-env (lambda-value-env lmb)));親にして、新しいenvを作る(自動で戻してくれる)
         (execute-body interp (block-value-items (lambda-value-body lmb))))))

    (define (call-proc! interp proc)
      (call-with-frame
       interp
       (proc-value-file proc)
       (proc-value-line proc)
       (value->write-string proc)
       (lambda ()
         (execute-body interp (block-value-items (proc-value-body proc))))))

    (define (invoke! interp call-func)
      (cond
       ((builtin-func? call-func) ((builtin-func-proc call-func) interp))
       ((lambda-value? call-func) (call-lambda! interp call-func) )
       ((proc-value? call-func) (call-proc! interp call-func))
       (else (interp-error! interp "TypeError" "not callable value is passd"))))

    (define (exec-block block interp);execute-bodyの超々薄いラッパーなので、もしかしたら統合するかも
      ;;blockを実行（外には渡さない）
      (execute-body interp (block-value-items block)))

    (define (apply-callable! interp value)
      (cond
       ((block-value? value)
        (exec-block value interp))
       ((lambda-value? value)
        (call-lambda! interp value))
       ((builtin-func? value)
        ((builtin-func-proc value) interp))
       ((proc-value? value)
        (call-proc! interp value))
       (else
        (interp-error! interp "TypeError" "expects callable"))))

    (define (execute-body interp sentence)
      (for-each
       (lambda (pair)
         (let ((item (car pair)) (item-line (cdr pair))) ;ここで分離
           (interp-token-line-set! interp item-line)
           (cond
            ((symbol-value? item)
             (if (in-env? (interp-env interp) (symbol-value-token item)) ;安全確認
                 (let ((v (env-get (interp-env interp) (symbol-value-token item))))
                   (if (or (builtin-func? v) (lambda-value? v) (proc-value? v))
                       (invoke! interp v)
                       (stack-push! interp v)))
                 (interp-error! interp "NameError" (string-append "unknown-name:" (symbol-value-token item)))))
            
            ((lazy-value? item)
             (stack-push! interp (parse-one-token (lazy-value-token item) item-line)))
            
            (else
             (stack-push! interp item)))))
       sentence))

    (define (parse-code code file callers)
      (guard (e ((and (mylang-error? e)) (mylang-error-resolve e file "<toplevel>" callers)))
        (parse-paren (sorting-types (lexer code)))))

    (define (line->string line)
      (if line (number->string line) "?"))

    (define (print-error e)
      (let ((port (current-error-port)))
        (newline port)
        (for-each (lambda (frame)
                    (display
                     (string-append "from "
                                    (frame-file frame)
                                    " at line "
                                    (line->string (frame-line frame))
                                    " in "
                                    (frame-name frame)
                                    "\n") port))
                  (reverse (mylang-error-trace e)))
        (display (string-append (mylang-error-name e)
                                ":"
                                (mylang-error-message e)) port)))

    (define (interp-run interp code file)
      (guard (e
              ((mylang-error? e)
               (print-error e)
               (exit 1)))
        (interp-file-set! interp file)
        (interp-name-set! interp "<toplevel>")
        (execute-body interp (parse-code code file '()))))

    (define (make-interp builtins);決まりきった引数を削り取ったやつ
      (let* ((env (make-env #f))
             (interp (make-interp-raw '() env #f "<unknown>" "<toplevel>" '())));初期化用の、空スタック、親がいないenv(外側)、lineは#f、call-stackは無 を作る。
        (for-each
         (lambda (entry);(name . proc)のリストをもらう
           (env-define env (car entry) (make-builtin-func (car entry) (cdr entry))))
         builtins)
        interp))));for-eachは返さないから、interpを返す。
