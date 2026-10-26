(define-library (mylang builtin math-func)
  (export math-func-dict)
  (import (scheme base)
	  (scheme write)
          (scheme inexact)
          (mylang values)
          (mylang interpreter))
  
  (begin
    (define (make-binary-math-func name opr)
      (lambda (interp)
        (let* ((a (stack-pop! interp))
               (b (stack-pop! interp)))
          (if (and (number? a) (number? b))
              (stack-push! interp (opr b a))
              (interp-error! interp
                             "TypeError"
                             (string-append
                              "the \"" name "\" func expects two numbers, but got "
                              (value->write-string b)
                              " and "
                              (value->write-string a)))))))

    (define (make-div-math-func name opr)
      (lambda (interp)
        (let* ((a (stack-pop! interp))
               (b (stack-pop! interp)))
          (if (and (number? a) (number? b))
              (if (zero? a)
                  (interp-error! interp "ZeroDivisionError" "division by zero")
                  (stack-push! interp (opr b a)))
              (interp-error! interp
                             "TypeError"
                             (string-append
                              "the \"" name "\" func expects two numbers, but got "
                              (value->write-string b)
                              " and "
                              (value->write-string a)))))))

    (define (make-unary-math-func name opr)
      (lambda (interp)
        (let* ((a (stack-pop! interp)))
          (if (and (number? a))
              (stack-push! interp (opr a))
              (interp-error! interp
                             "TypeError"
                             (string-append
                              "the \"" name "\" func expects one numbers, but got "
                              (value->write-string a)))))))
    
  (define (sqrt-func interp)
    (let* ((a (stack-pop! interp)))
      (if (and (number? a) (<= 0 a))
          (stack-push! interp (sqrt a))
          (interp-error! interp "TypeError" (string-append "the \"sqrt\" func expects a non-negative numbers, but got "
                                                           (value->write-string a))))))
  (define math-func-dict
    `(("+" . ,(make-binary-math-func "+" +))
      ("-" . ,(make-binary-math-func "-" -))
      ("*" . ,(make-binary-math-func "*" *))
      ("/" . ,(make-div-math-func "/" /))
      ("%" . ,(make-div-math-func "%" modulo))
      ("**" . ,(make-binary-math-func "**" expt))
      ("//" . ,(make-div-math-func "//" (lambda (a b) (floor (/ a b)))))
      ("floor" . ,(make-unary-math-func "floor" floor))
      ("sqrt" . ,sqrt-func)
      ("abs" . ,(make-unary-math-func "abs" abs))))))

