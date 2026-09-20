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

    (define (each-func interp)
      (let* ((proc (stack-pop! interp))
             (lst (stack-pop! interp)))
        (if (not (block-value? lst))
            (interp-error! interp  "TypeError" "the \"each\" func excepts a block as second arg.")
            (let loop ((items (block-value-items lst)))
              (if (null? items)
                  '();何も返さない
                  (begin
                    (stack-push! interp (car (car items)))
                    (apply-callable! interp proc)
                    (loop (cdr items))))))))

    (define (filter-func interp)
      (let* ((proc (stack-pop! interp))
             (lst (stack-pop! interp)))
        (if (not (block-value? lst))
            (interp-error! interp "TypeError" "the" "\"filter\" func expects a block as second arg.")
            (let ((items (block-value-items lst)))
              (let loop ((rest items) (acc '()))
                (if (null? rest)
                    (stack-push! interp (make-block-value (reverse acc)))
                    (begin
                      (stack-push! interp (car (car rest)))
                      (apply-callable! interp proc)
                      (let ((result (stack-pop! interp)))
                        (if (or (eq? result #f) (eq? result the-nil))
                            (loop (cdr rest) acc)
                            (loop (cdr rest) (cons (car rest) acc)))))))))))



    (define higher-order-func-dict
      `(("map" . ,map-func)
        ("each" . , each-func)
        ("filter" . ,filter-func)))))
