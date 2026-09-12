(define-library (mylang env)
  (export
   make-env
   env?
   in-env?
   env-define
   env-set!
   env-get)
  (import (scheme base)
	  (scheme write))

  (begin
    (define-record-type <env>
      (make-env-raw vars parent)
      env?
      (vars env-vars env-vars-set!)
      (parent env-parent))

    (define (make-env parent)
      (make-env-raw '() parent))

    (define (in-env? env name)
      (cond ((assoc name (env-vars env)) #t) ;そのまま自分が持ってるなら#tを返す
	    ((env-parent env) (in-env? (env-parent env) name));親がいるなら、親に探してもらう
	    (else #f))) ;なかった!

    (define (env-define env name value)
      (env-vars-set! env
        (cons (cons name value) (env-vars env))))

    (define (env-set! env name value)
      (let loop ((now-env env))
	(if (not now-env) ;親がいないなら
	    #f
	    (if (assoc name (env-vars now-env))
		(begin
		  (set-cdr! (assoc name (env-vars now-env)) value)
		  #t);成功(#t)
		(loop (env-parent now-env))))))


    (define (env-get env name)
      (let loop ((now-env env))
	(if (not now-env) ;親がいないなら
	    (begin (write (env-vars env))(write name)(error "unknownname"));とりあえず適当にプリント（debug
	    (if (assoc name (env-vars now-env))
		(cdr (assoc name (env-vars now-env)))
		(loop (env-parent now-env))))))))
