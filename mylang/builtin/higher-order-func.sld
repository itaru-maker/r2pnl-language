(define-library (mylang builtin higher-order-func)
  (export higher-order-func-dict)
  (import (scheme base)
          (scheme write)
          (mylang values)
          (mylang interpreter))

  (begin
    (define (map-func interp)
      (let* ((proc (stack-pop! interp))
             (lst (stack-pop! interp)))
        (if (not (block-value? lst))
            (interp-error! interp  "TypeError" "the \"map\" func excepts a block as second arg.")
            (let loop ((items (block-value-items lst)) (acc '()))
              (if (null? items)
                  (stack-push! interp (make-block-value (reverse acc)))
                  (begin
                    (stack-push! interp (car (car items)))
                    (apply-callable! interp proc)
                    (loop (cdr items)
                           (cons (cons (stack-pop! interp) (interp-token-line interp)) acc))))))))
    (define higher-order-func-dict
      `(("map" . ,map-func)))))
