(define-library (mylang builtin math-func)
  (export math-func-dict)
  (import (scheme base)
	  (scheme write)
          (scheme inexact)
          (mylang values)
	  (mylang interpreter))
  
  (begin
    (define (add-func interp)
      (let* ((a (stack-pop! interp))
	     (b (stack-pop! interp)))
        (if (and (number? a) (number? b))
	    (stack-push! interp (+ a b))
	    (interp-error! interp "TypeError" (string-append "the \"+\" func expects two numbers, but got "
                                                (value->write-string b) " and " (value->write-string a))))))

    (define (sub-func interp)
      (let* ((a (stack-pop! interp))
	     (b (stack-pop! interp)))
        (if (and (number? a) (number? b))
	    (stack-push! interp (- b a))
	    (interp-error! interp "TypeError" (string-append "the \"-\" func expects two numbers, but got "
                                                (value->write-string b) " and " (value->write-string a))))))

    (define (mul-func interp)
      (let* ((a (stack-pop! interp))
	     (b (stack-pop! interp)))
        (if (and (number? a) (number? b))
	    (stack-push! interp (* b a))
	    (interp-error! interp "TypeError" (string-append "the \"*\" func expects two numbers, but got "
                                                 (value->write-string b) " and " (value->write-string a))))))

  (define (div-func interp)
    (let* ((a (stack-pop! interp))
	   (b (stack-pop! interp)))
      (if (and (number? a) (number? b))
          (if (zero? a)
              (interp-error! interp "ZeroDivisionError" "division by zero")
	      (stack-push! interp (/ b a)))
	  (interp-error! interp "TypeError" (string-append  "the \"/\" func expects two numbers, but got "
                                                 (value->write-string b) " and " (value->write-string a))))))


  (define (mod-func interp)
    (let* ((a (stack-pop! interp))
	   (b (stack-pop! interp)))
      (if (and (number? a) (number? b))
          (if (zero? a)
              (interp-error! interp "ZeroDivisionError" "modulo by zero")
	      (stack-push! interp (modulo b a)))
	  (interp-error! interp "TypeError" (string-append  "the \"%\" func expects two numbers, but got "
                                                           (value->write-string b) " and " (value->write-string a))))))

    (define (expt-func interp)
      (let* ((a (stack-pop! interp))
	     (b (stack-pop! interp)))
        (if (and (number? a) (number? b))
	    (stack-push! interp (expt b a))
	    (interp-error! interp "TypeError" (string-append "the \"**\" func expects two numbers, but got "
                                                             (value->write-string b) " and " (value->write-string a))))))

  (define (floor-div-func interp)
    (let* ((a (stack-pop! interp))
	   (b (stack-pop! interp)))
      (if (and (number? a) (number? b))
          (if (zero? a)
              (interp-error! interp "ZeroDivisionError" "floor-div by zero")
	      (stack-push! interp (floor (/ b a))))
	  (interp-error! interp "TypeError" (string-append  "the \"//\" func expects two numbers, but got "
                                                           (value->write-string b) " and " (value->write-string a))))))


  (define (int-func interp)
    (let* ((a (stack-pop! interp)))
      (if (number? a)
          (stack-push! interp (floor a))
          (interp-error! interp "TypeError" (string-append "the \"int\" func expects one numbers, but got "
                                               (value->write-string a))))))

  (define (neg-func interp)
    (let* ((a (stack-pop! interp)))
      (if (number? a)
          (stack-push! interp (- a))
          (interp-error! interp "TypeError" (string-append "the \"neg\" func expects one numbers, but got "
                                                           (value->write-string a))))))

  (define (sqrt-func interp)
    (let* ((a (stack-pop! interp)))
      (if (and (number? a) (< 0 a))
          (stack-push! interp (sqrt a))
          (interp-error! interp "TypeError" (string-append "the \"sqrt\" func expects one positive numbers, but got "
                                                           (value->write-string a))))))
  
  (define (abs-func interp)
    (let* ((a (stack-pop! interp)))
      (if (number? a)
          (stack-push! interp (abs a))
          (interp-error! interp "TypeError" (string-append "the \"abs\" func expects one numbers, but got "
                                               (value->write-string a))))))
  
  (define (even?-func interp)
    (let* ((a (stack-pop! interp)))
      (if (number? a)
          (stack-push! interp (even? a))
          (interp-error! interp "TypeError" (string-append "the \"even\" func expects one numbers, but got "
                                               (value->write-string a))))))


  (define (odd?-func interp)
    (let* ((a (stack-pop! interp)))
      (if (number? a)
          (stack-push! interp (odd? a))
          (interp-error! interp "TypeError" (string-append "the \"odd\" func expects one numbers, but got "
                                                           (value->write-string a))))))




  (define math-func-dict
    `(("+" . ,add-func)
      ("-" . ,sub-func)
      ("*" . ,mul-func)
      ("/" . ,div-func)
      ("%" . ,mod-func)
      ("**" . ,expt-func)
      ("//" . ,floor-div-func)
      ("int" . ,int-func)
      ("neg" . ,neg-func)
      ("sqrt" . ,sqrt-func)
      ("abs" . ,abs-func)
      ("even?" . ,even?-func)
      ("odd?" . ,odd?-func)))))
  
