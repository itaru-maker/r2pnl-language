(define-library (mylang builtin stack-func)
  (export stack-func-dict)
  (import (scheme base)
	  (scheme write)
	  (mylang interpreter))

  (begin
    (define (pick-func interp)
      (let* ((i (stack-pop! interp)))
        (cond ((not (and (number? i) (integer? i) (<= 0 i)))
               (interp-error! interp "TypeError" "the \"pick\" func expects a non-negative integer"))
              ((<= (length (interp-stack interp)) i)
               (interp-error! interp "StackUnderflowError" "the \"pick\" func index out of range"))
              (else (let ((v (list-ref (interp-stack interp) (exact i))))
                      (stack-push! interp v))))))

    (define (roll-func interp)
      (let* ((num (stack-pop! interp)))
        (cond ((not (and (number? num) (integer? num) (<= 0 num)))
               (interp-error! interp "TypeError" "the \"roll\" func expects a non-negative integer"))
              ((<= (length (interp-stack interp)) num)
               (interp-error! interp "StackUnderflowError" "the \"roll\" func index out of range"))
              (else
               (let loop ((i 0)
                          (cur (car (interp-stack interp)))
                          (acc '())
                          (rest (cdr (interp-stack interp))))
                 (if (<= num i)
                     (interp-stack-set! interp (append (cons cur (reverse acc)) rest))
                     (loop (+ i 1) (car rest) (cons cur acc) (cdr rest))))))))
    
     (define (drop-func interp)
      (stack-pop! interp))

    (define stack-func-dict
      `(("pick" . ,pick-func)
        ("roll" . ,roll-func)
        ("drop" . ,drop-func)))))
