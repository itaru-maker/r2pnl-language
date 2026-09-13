(define-library (mylang builtin math-func)
  (export math-func-dict)
  (import (scheme base)
	  (scheme write)
          (mylang values)
	  (mylang interpreter))
  (begin
    (define (add-func interp)
      (let* ((a (stack-pop! interp))
	     (b (stack-pop! interp)))
        (if (and (number? a) (number? b))
	    (stack-push! interp (+ a b))
	    (interp-error! interp "TypeError" (string-append "the \"add\" func expects two numbers, but got "
                                                (value->write-string b) " and " (value->write-string a))))))

    (define (sub-func interp)
      (let* ((a (stack-pop! interp))
	     (b (stack-pop! interp)))
        (if (and (number? a) (number? b))
	    (stack-push! interp (- b a))
	    (interp-error! interp "TypeError" (string-append "the \"sub\" func expects two numbers, but got "
                                                (value->write-string b) " and " (value->write-string a))))))

    (define (mul-func interp)
      (let* ((a (stack-pop! interp))
	     (b (stack-pop! interp)))
        (if (and (number? a) (number? b))
	    (stack-push! interp (* b a))
	    (interp-error! interp "TypeError" (string-append "the \"mul\" func expects two numbers, but got "
                                                 (value->write-string b) " and " (value->write-string a))))))

  (define (div-func interp)
    (let* ((a (stack-pop! interp))
	   (b (stack-pop! interp)))
      (if (and (number? a) (number? b))
          (if (zero? a)
              (interp-error! interp "ZeroDivisionError" "division of zero")
	      (stack-push! interp (/ b a)))
	  (interp-error! interp "TypeError" (string-append  "the \"sub\" func expects two numbers, but got "
                                                 (value->write-string b) " and " (value->write-string a))))))


  (define (mod-func interp)
    (let* ((a (stack-pop! interp))
	   (b (stack-pop! interp)))
      (if (and (number? a) (number? b))
          (if (zero? a)
              (interp-error! interp "ZeroDivisionError" "modulo of zero")
	      (stack-push! interp (modulo b a)))
	  (interp-error! interp "TypeError" (string-append  "the \"mod\" func expects two numbers, but got "
                                                           (value->write-string b) " and " (value->write-string a))))))

    (define (expt-func interp)
      (let* ((a (stack-pop! interp))
	     (b (stack-pop! interp)))
        (if (and (number? a) (number? b))
	    (stack-push! interp (expt b a))
	    (interp-error! interp "TypeError" (string-append "the \"expt\" func expects two numbers, but got "
                                                 (value->write-string b) " and " (value->write-string a))))))
  
  ;;floor-divはあとで

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
    `(("add" . ,add-func)
      ("sub" . ,sub-func)
      ("mul" . ,mul-func)
      ("div" . ,div-func)
      ("mod" . ,mod-func)
      ("expt" . ,expt-func)
      ("+" . ,add-func)
      ("-" . ,sub-func)
      ("*" . ,mul-func)
      ("/" . ,div-func)
      ("%" . ,mod-func)
      ("**" . ,expt-func)
      ("int" . ,int-func)
      ("neg" . ,neg-func)
      ("even?" . ,even?-func)
      ("odd?" . ,odd?-func)))))
  
