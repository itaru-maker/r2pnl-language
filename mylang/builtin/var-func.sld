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
		(env-define (interp-env interp)
                            (symbol-value-token name)
                            (if (or (proc-value? value) (lambda-value? value))
                                (begin (name-callable value (symbol-value-token name))
                                       value)
                                value)))
	    (interp-error!
	     interp "TypeError" (string-append "the \"let\" func expect symbol-value and any-value, but value "
					       (value->write-string name)
					       " is passed as symbol")))))
    
    (define (set-func interp)
      (let* ((value (stack-pop! interp))
	     (name (stack-pop! interp)))
	(if (symbol-value? name)
	    (if (in-env? (interp-env interp) (symbol-value-token name))
		(env-set! (interp-env interp) (symbol-value-token name)
                          (if (or (lambda-value? value) (proc-value? value))
                              (begin (name-callable value (symbol-value-token name))
                                     value)
                              value))
		(interp-error! interp "UndefinedError" (string-append
                                                         "the \"set\" func expects a defined symbol, but \""
                                                         (symbol-value-token name)
                                                         "\" is not defined yet") ))
	    (interp-error! interp "TypeError" (string-append "func \"set\" expect symbol-value and any-value, but value "
							     (value->write-string name)
							     " is passed as symbol")))))

    (define (get-func interp)
      (let* ((name (stack-pop! interp)))
        (if (symbol-value? name)
            (let ((result (env-lookup (interp-env interp) (symbol-value-token name))))
              (if result
                  (stack-push! interp (cdr result))
                  (interp-error! interp "UndefinedError" (string-append
                                                          "the \"get\" func expects a defined symbol, but \""
                                                          (symbol-value-token name)
                                                          "\" is not defined yet") )))
            (interp-error! interp "TypeError" (string-append "func \"get\" expect symbol-value, but value "
							     (value->write-string name)
							     " is passed as symbol")))))
    
    (define var-func-dict
      `(("let" . ,let-func)
	("set" . ,set-func)
        ("get" . ,get-func)))))
