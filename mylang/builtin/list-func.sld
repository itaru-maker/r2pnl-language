(define-library (mylang builtin list-func)
  (export list-func-dict)
  (import (scheme base)
          (mylang values)
          (mylang tokens)
          (mylang interpreter))

  (begin
    ;;動的に配列を作りやすくするために作った。（{ }はparse時にやるので { 2 3 + } が3要素になってしまう ）
    (define (list-marker-func interp)
      (stack-push! interp the-list-marker))

    (define (make-list-func interp)
      (let loop ((acc '()))
        (let ((v (stack-pop! interp)))
          (if (list-marker? v)
              (stack-push! interp (make-block-value
                            (map (lambda (x) (cons x (interp-token-line interp)))
                                 acc)))
              (loop (cons v acc))))))

    (define list-func-dict
      `(("[" . ,list-marker-func)
        ("]" . ,make-list-func)))))

