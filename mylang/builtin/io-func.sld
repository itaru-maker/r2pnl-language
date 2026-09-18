(define-library (mylang builtin io-func)
  (export io-func-dict)
  (import (scheme base)
	  (scheme write)
	  (mylang values)
	  (mylang interpreter))
  (begin
      (define  (write-func interp)
	(display
	 (value->write-string
	  (stack-pop! interp))))

      (define (print-func interp)
        (display
         (value->display-string
          (stack-pop! interp))))

      (define (newline-func interp)
        (display "\n"))

      (define (write-stack-func interp)
        (display "\n=====debug=====\nbottom\n")
        (for-each (lambda (item) (display (value->write-string item)) (newline))
                  (reverse (interp-stack interp)))
        (display "top\n==============="))

      (define (input-func interp)
        (flush-output-port)
        (let ((input (read-line)))
          (stack-push! interp (if (eof-object? input) "" input))))

      (define io-func-dict
	`(("." . ,write-func)
          ("print" . ,print-func)
          ("newline" . ,newline-func)
          (".s" . ,write-stack-func)
          ("input" . ,input-func)))))
