(define-library (mylang values)
  (export
   nil-value? the-nil ;にlは一つでいいので
   make-symbol-value symbol-value? symbol-value-token
   make-lazy-value lazy-value? lazy-value-token
   make-block-value block-value? block-value-items block-value-items-set!
   block-value-append!
   make-builtin-func builtin-func? builtin-func-name builtin-func-proc
   make-lambda-value lambda-value? lambda-value-body lambda-value-env lambda-value-line 
   value->write-string
   value->display-string)
  (import (scheme base)
	  (scheme write)
	  (mylang tokens))
  (begin
    ;;float str true false はschemeの機能を借りる
    ;;nil symbol lazy block lambdaは自作する
    (define-record-type <nil-value>
      (make-nil-value)
      nil-value?)
    (define the-nil (make-nil-value)) ; nil唯一のインスタンス
    
    (define-record-type <symbol-value>
      (make-symbol-value token)
      symbol-value?
      (token symbol-value-token))

    (define-record-type <lazy-value>
      (make-lazy-value token)
      lazy-value?
      (token lazy-value-token))

    (define-record-type <block-value>
      (make-block-value items)
      block-value?
      (items block-value-items block-value-items-set!))

    (define (block-value-append! block item)
      (block-value-items-set! block
        (cons item (block-value-items block))))

    
    (define-record-type <builtin-func>
      (make-builtin-func name proc)
      builtin-func?
      (name builtin-func-name)
      (proc builtin-func-proc))

    (define-record-type <lambda-value>
      (make-lambda-value body env line)
      lambda-value?
      (body lambda-value-body)
      (env lambda-value-env)
      (line lambda-value-line))


    (define (value->write-string value)
      (cond ((number? value) (number->string value))
	    ((string? value) (string-append "\"" value "\""))
	    ((boolean? value) (if value "#true" "#false"))
	    ((nil-value? value) "#nil")
	    ((symbol-value? value) (symbol-value-token value))
	    ((lazy-value? value) (string-append "'" (lazy-value-token value)))
            ((block-value? value)
             (string-append "{ "
			    (apply string-append (map (lambda (pair) (string-append (value->write-string (car pair)) " ")) (block-value-items value)))
			    "}"))
	    ((builtin-func? value)
	     (string-append "<builtin:" (builtin-func-name value) ">"))
	     ((lambda-value? value)
	      (string-append "<lambda" (if (lambda-value-line value) (string-append " at " (number->string (lambda-value-line value)))) ">"))
	    ;;tokens
	    ((r-paren? value) "<r-paren>")
	    ((l-paren? value) "<l-paren>")
            ((list-marker? value) "<list-marker>")
	
  	    (else (write "debug:unknown-type") (write  value))))

    ;;value->write-stringと比べて、string以外ほぼ変わってないので存在意義があるのかは不明
    (define (value->display-string value)
      (cond ((number? value) (number->string value))
	    ((string? value) value)
	    ((boolean? value) (if value "#true" "#false"))
	    ((nil-value? value) "#nil")
	    ((symbol-value? value) (symbol-value-token value))
	    ((lazy-value? value) (string-append "'" (lazy-value-token value)))
            ((block-value? value)
             (string-append "{ "
			    (apply string-append (map (lambda (pair) (string-append (value->display-string (car pair)) " ")) (block-value-items value)))
                            "}"))
	    ((builtin-func? value)
	     (string-append "<builtin:" (builtin-func-name value) ">"))
	     ((lambda-value? value)
	      (string-append "<lambda" (if (lambda-value-line value) (string-append " at " (number->string (lambda-value-line value)))) ">"))
	    ;;tokens
	    ((r-paren? value) "<r-paren>")
	    ((l-paren? value) "<l-paren>")
            ((list-marker? value) "<list-marker>")
	
  	    (else (write "debug:unknown-type") (write  value))))))
