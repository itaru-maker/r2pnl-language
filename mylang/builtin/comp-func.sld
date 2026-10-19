(define-library (mylang builtin comp-func)
  (export comp-func-dict)
  (import (scheme base)
          (mylang values)
          (mylang tokens)
          (mylang interpreter))
  (begin
    (define (eq?-func interp)
      (let*
          ((a (stack-pop! interp))
           (b (stack-pop! interp)))
        (stack-push! interp (eq? a b))))

    (define (every proc lst1 lst2)
      (cond
       ((and (null? lst1) (null? lst2)) #t)
       ((or (null? lst1) (null? lst2)) #f)
       ((proc (car lst1) (car lst2))
        (every proc (cdr lst1) (cdr lst2)));再帰的に呼ぶ
       (else #f)))
    
    (define (value-equal? a b)
      (cond
       ((and (number? a) (number? b))
        (equal? a b))
       
       ((and (string? a) (string? b))
        (equal? a b))
       
       ((and (eq? a #t) (eq? b #t))
        #t)

       ((and (not a) (not b))
        #t)
           
       ((and (nil-value? a) (nil-value? b))
        #t)

       ((and (symbol-value? a) (symbol-value? b))
        (equal? (symbol-value-token a) (symbol-value-token b)))
       ;;わざわざtoken取らなくてもいいのかな

       ((and (quote-value? a) (quote-value? b))
        (equal? (quote-value-token a ) (quote-value-token b)))
       
       ((and (block-value? a) (block-value? b));違う行のものでもしっかり比較されるように修正済み
        (let ((items-a (block-value-items a)) (items-b (block-value-items b)))
          (and (= (length items-a) (length items-b))
               (every
                (lambda (pair1 pair2)
                  (value-equal? (car pair1) (car pair2)));再帰的に呼ぶ
                items-a
                items-b))))
           
       ((and (builtin-func? a) (builtin-func? b))
        (eq? a b))

       ((and (lambda-value? a) (lambda-value? b))
        (eq? a b))

       ((and (proc-value? a) (proc-value? b))
        (eq? a b))
           
       ((and (r-paren? a) (r-paren? b))
        #t)
           
       ((and (l-paren? a) (l-paren? b))
        #t)

       (else #f)))

    (define (equal?-func interp)
      (let* ((a (stack-pop! interp))
             (b (stack-pop! interp)))
        (stack-push! interp (value-equal? a b))))

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

    (define comp-func-dict
      `(("eq?" . ,eq?-func)
        ("equal?" . ,equal?-func)
        ("=" . ,(make-binary-math-func "=" =))
        ("/=" . ,(make-binary-math-func "/=" (lambda (a b) (not (= a b)))))
        ("<" . ,(make-binary-math-func "<" <))
        (">" . ,(make-binary-math-func ">" >))
        ("<=" . ,(make-binary-math-func "<=" <=))
        (">=" . ,(make-binary-math-func ">=" >=))))))
