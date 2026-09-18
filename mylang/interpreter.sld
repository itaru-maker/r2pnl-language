(define-library (mylang interpreter)
  (export
   make-interp make-interp-raw interp? interp-stack interp-stack-set! interp-env interp-env-set! interp-token-line interp-token-line-set!
   stack-push! stack-pop!
   interp-error!
   call-lambda!
   invoke!
   exec-block
   apply-callable!
   interp-run)
  
  (import
   (scheme base)
   (scheme write)
   (mylang values)
   (mylang tokens)
   (mylang env)
   (mylang lexar)
   (mylang parser)
   (mylang error))
  
  (begin
    (define-record-type <interp>
      (make-interp-raw stack env token-line)
      interp?
      (stack interp-stack interp-stack-set!)
      (env interp-env interp-env-set!)
      (token-line interp-token-line interp-token-line-set!))

    (define (stack-push! interp value)
      (interp-stack-set! interp (cons value (interp-stack interp))))
    
    (define (stack-pop! interp)
      (let ((s (interp-stack interp)))
	(if (null? s)
	    (interp-error! interp "StackOverflowError" "pop from empty stack")
	    (begin (interp-stack-set! interp (cdr s))
		   (car s)))))
    
    (define (interp-error! interp error-name message . opt-line);builtinはこいつを投げる
      (let ((error-line (if (not (null? opt-line)) (car opt-line) (interp-token-line interp))))
	(raise (make-mylang-error error-name message error-line '())))) ;一番最初に起動した時だから、traceは空

    (define (call-lambda! interp lmb)
      (let*
          ((lambda-env (make-env(lambda-value-env lmb)));lambdaの中のenv
           (caller-line (interp-token-line interp))
           (caller-env (interp-env interp)));interpが元々持っていたenv

        (guard (e
                ((mylang-error? e)
                 (mylang-error-trace-set!
                  e
                  (cons (string-append "at line" (number->string caller-line) "\n")
                        (mylang-error-trace e)))
                 (interp-env-set! interp caller-env)
                 (interp-token-line-set! interp caller-line)
                 (raise e)));終わったら、そのまま上に流す

          ;;切り替え！
          (interp-env-set! interp lambda-env)
          (interp-token-line-set! interp (lambda-value-line lmb))

          (execute-body interp (block-value-items (lambda-value-body lmb)));ここで実行

          (interp-env-set! interp caller-env);戻す
          (interp-token-line-set! interp caller-line))))

    (define (invoke! interp call-func)
      (cond
       ((builtin-func? call-func) ((builtin-func-proc call-func) interp))
       ((lambda-value? call-func) (call-lambda! interp call-func) )
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
                   (if (or (builtin-func? v) (lambda-value? v))
                       (invoke! interp v)
                       (stack-push! interp v)))
		 (interp-error! interp "NameError" (string-append "unknown-name:" (symbol-value-token item)))))
            
	    ((lazy-value? item)
	     (stack-push! interp (parse-one-token (lazy-value-token item) item-line)))
            
	    (else
	     (stack-push! interp item)))))
       sentence))

    
    (define (interp-run interp code)
      (guard (e;エラーをキャッチ
	      ((mylang-error? e)
               (newline)
               (display "-====ERROR====-")
               (newline)
	       (for-each (lambda (x) (display x))
			 (reverse (mylang-error-trace e)))
	       (display (string-append (mylang-error-name e) ":" (mylang-error-message e) " at line " (number->string (mylang-error-line e))))))

	(let*
	    ((raw-tokens (lexar code))
	     (types (sorting-types raw-tokens))
	     (structure-types (parse-paren types)))
          (execute-body interp structure-types))))

    (define (make-interp builtins);決まりきった引数を削り取ったやつ
      (let* ((env (make-env #f))
	     (interp (make-interp-raw '() env #f)));初期化用の、空スタック、親がいないenv(外側)、lineは#f を作る。
	(for-each
	 (lambda (entry);(name . proc)のリストをもらう
	   (env-define env (car entry) (make-builtin-func (car entry) (cdr entry))))
	 builtins)
	interp))));for-eachは返さないから、interpを返す。
