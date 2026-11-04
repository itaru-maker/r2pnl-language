(define-library (mylang builtin var-func)
  (export var-func-dict)
  (import (scheme base)
	  (mylang values)
	  (mylang interpreter)
	  (mylang env))
  (begin
    (define  (fn-func interp)
      (let* ((block (stack-pop! interp))
	     (name (stack-pop! interp)))
	(if (and (symbol-value? name) (block-value? block))
	    (if (in-env? (interp-env interp) (symbol-value-token name))
		(interp-error! interp "AlreadyDefinedError" (string-append
                                                             "the \"fn\" func expects an undefined symbol, but \""
                                                             (symbol-value-token name)
                                                             "\" is already defined!"))
		(env-define (interp-env interp)
                            (symbol-value-token name)
                            (make-proc-value
                             block
                             (interp-token-line interp)
                             (interp-file interp)
                             (symbol-value-token name))))
	    (interp-error!
	     interp "TypeError" "the \"fn\" func expect a symbol and a block "))))
    
    (define (redefine-func interp)
      (let* ((block (stack-pop! interp))
	     (name (stack-pop! interp)))
	(if (or (symbol-value? name) (block-value? block)) 
	    (if (in-env? (interp-env interp) (symbol-value-token name))
		(env-set! (interp-env interp)
                          (symbol-value-token name)
                          (make-proc-value
                           block
                           (interp-token-line interp)
                           (interp-file interp)
                           (symbol-value-token name)))
		(interp-error! interp "UndefinedError" (string-append
                                                         "the \"redefine\" func expects a defined symbol, but \""
                                                         (symbol-value-token name)
                                                         "\" is not defined yet") ))
	    (interp-error! interp "TypeError" "the \"redefine\" func expect a symbol and a block"))))

    (define (get-func interp)
      (let* ((name (stack-pop! interp)))
        (if (symbol-value? name)
            (let ((result (env-lookup (interp-env interp) (symbol-value-token name))))
              (if result
                  (stack-push! interp (proc-value-body (cdr result)))
                  (interp-error! interp "UndefinedError" (string-append
                                                          "the \"get\" func expects a defined symbol, but \""
                                                          (symbol-value-token name)
                                                          "\" is not defined yet") )))
            (interp-error! interp "TypeError" (string-append "func \"get\" expect symbol-value, but value "
							     (value->write-string name)
							     " is passed as symbol")))))

    (define (var-func interp)
      (let* ((v (stack-pop! interp))
             (name (stack-pop! interp)))
        (if (not (symbol-value? name))
            (interp-error!
	     interp "TypeError" "the \"var\" func expect a symbol and a value ")
            (env-define (interp-env interp) (symbol-value-token name) v))))

    (define (set-func interp)
      (let* ((v (stack-pop! interp))
             (name (stack-pop! interp)))
        (if (not (symbol-value? name))
            (interp-error!
	     interp "TypeError" "the \"set\" func expect a symbol and a value ")
            (env-set! (interp-env interp) (symbol-value-token name) v))))

    (define (forget-func interp)
      (let* ((name (stack-pop! interp)))
        (if (not (symbol-value? name))
            (interp-error!
	     interp "TypeError" "the \"forget\" func expect a symbol")
            (env-delete! (interp-env interp) (symbol-value-token name)))))
    
    
    (define var-func-dict
      `(("fn" . ,fn-func)
	("redefine" . ,redefine-func)
        ("get" . ,get-func)
        ("var" . ,var-func)
        ("set" . ,set-func)
        ("forget" . ,forget-func)))))
