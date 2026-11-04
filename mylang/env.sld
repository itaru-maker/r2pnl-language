(define-library (mylang env)
  (export
   make-env
   env?
   in-env?
   env-lookup
   env-define
   env-set!
   env-get
   env-delete!)
  (import (scheme base)
	  (scheme write))

  (begin
    (define-record-type <env>
      (make-env-raw vars)
      env?
      (vars env-vars env-vars-set!))

    (define (make-env)
      (make-env-raw '()))

    (define (in-env? env name)
      (if (assoc name (env-vars env)) #t #f))

    (define (env-lookup env name)
      (assoc name (env-vars env)))

    (define (env-define env name value)
      (env-vars-set! env
        (cons (cons name value) (env-vars env))))

    (define (env-set! env name value)
      (let ((pair (assoc name (env-vars env))))
        (if pair
            (set-cdr! pair value))))


    (define (env-get env name)
      (if (assoc name (env-vars env))
	  (cdr (assoc name (env-vars env)))
	  #f))
    
    (define (env-delete! env name)
      (let loop ((rest (env-vars env)) (acc '()))
        (cond
         ((null? rest) #f)
         ((equal? name (caar rest))
          (env-vars-set! env (append (reverse acc) (cdr rest))) #t)
         (else (loop (cdr rest) (cons (car rest) acc))))))

    (define (env-snapshot env) (env-vars env))))





