(define-library (mylang builtin var-func)
  (export var-func-dict)
  (import (scheme base)
	  (mylang values)
	  (mylang interpreter)
	  (mylang env))
  (begin
    (define  (let-func interp)
      (let* ((value (stack-pop! interp))
	     (name (stack-pop! interp)))
	(if (symbol-value? name)
	    (if (in-env? (interp-env interp) (symbol-value-token name))
		(interp-error! interp "AlreadyDefinedError" (string-append
                                                             "the \"let\" func expects an undefined symbol, but \""
                                                             (symbol-value-token name)
                                                             "\" is already defined!"))
		(env-define (interp-env interp) (symbol-value-token name) value))
	    (interp-error!
	     interp "TypeError" (string-append "the \"let\" func expect symbol-value and any-value, but value "
					       (value->write-string name)
					       " is passed as symbol")))))
    
    (define (set-func interp)
      (let* ((value (stack-pop! interp))
	     (name (stack-pop! interp)))
	(if (symbol-value? name)
	    (if (in-env? (interp-env interp) (symbol-value-token name))
		(env-set! (interp-env interp) (symbol-value-token name) value)
		(interp-error! interp "UnderfinedError" (string-append
                                                         "the \"set\" func expects a defined symbol, but \""
                                                         (symbol-value-token name)
                                                         "\" is not defined yet") ))
	    (interp-error! interp "TypeError" (string-append "func \"set\" expect symbol-value and any-value, but value "
							     (value->write-string name)
							     " is passed as symbol")))))

    (define (deref-func interp)
      (let* ((name (stack-pop! interp)))
        (if (symbol-value? name)
            (if (in-env? (interp-env interp)(symbol-value-token name))
                (stack-push! interp (env-get (interp-env interp) (symbol-value-token name)))
                (interp-error! interp "UnderfinedError" (string-append
                                                         "the \"deref\" func expects a defined symbol, but \""
                                                         (symbol-value-token name)
                                                         "\" is not defined yet") ))
            (interp-error! interp "TypeError" (string-append "func \"deref\" expect symbol-value, but value "
							     (value->write-string name)
							     " is passed as symbol")))))
    
    (define var-func-dict
      `(("let" . ,let-func)
	("set" . ,set-func)
        ("deref" . ,deref-func)))))
