(define-library (mylang builtin comp-func)
  (export comp-func-dict)
  (import (scheme base)
          (mylang values)
          (mylang tokens)
          (mylang interpreter))
  (begin
    (define (is-func interp)
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

       ((and (lazy-value? a) (lazy-value? b))
        (equal? (lazy-value-token a ) (lazy-value-token b)))
       
       ((and (block-value? a) (block-value? b));違う行のものでもしっかり比較されるように修正済み
        (let ((items-a (block-value-items a)) (items-b (block-value-items b)))
          (and (= (length items-a) (length items-b))
               (every
                (lambda (pair1 pair2)
                  (value-equal? (car pair1) (car pair2)));再帰的に呼ぶ
                items-a
                items-b))))
           
       ((and (builtin-func? a) (builtin-func? b))
        (equal? a b))

       ((and (lambda-value? a) (lambda-value? b))
        (equal? a b))

       ((and (proc-value? a) (proc-value? b))
        (equal? a b))
           
       ((and (r-paren? a) (r-paren? b))
        #t)
           
       ((and (l-paren? a) (l-paren? b))
        #t)

       (else #f)))

    (define (equal-func interp)
      (let* ((a (stack-pop! interp))
             (b (stack-pop! interp)))
        (stack-push! interp (value-equal? a b))))

    (define (eq-number-func interp)
      (let* ((a (stack-pop! interp))
             (b (stack-pop! interp)))
        (if (and (number? a) (number? b))
            (stack-push! interp (= a b))
            (interp-error! interp "TypeError" "the \"=\" func expects two number args"))))

    (define (ne-number-func interp)
      (let* ((a (stack-pop! interp))
             (b (stack-pop! interp)))
        (if (and (number? a) (number? b))
            (stack-push! interp (not (= a b)))
            (interp-error! interp "TypeError" "/= func expects two number args"))))

    
    (define (lt-func interp)
      (let* ((num-1 (stack-pop! interp))
             (num-2 (stack-pop! interp)))
        (if (not (and (number? num-1) (number? num-2)))
            (interp-error! interp "TypeError" "< func expects two number args")
            (stack-push! interp (< num-2 num-1)))))

    (define (gt-func interp)
      (let* ((num-1 (stack-pop! interp))
             (num-2 (stack-pop! interp)))
        (if (not (and (number? num-1) (number? num-2)))
            (interp-error! interp "TypeError" "> func expects two number args")
            (stack-push! interp (> num-2 num-1)))))

    (define (le-func interp)
      (let* ((num-1 (stack-pop! interp))
             (num-2 (stack-pop! interp)))
        (if (not (and (number? num-1) (number? num-2)))
            (interp-error! interp "TypeError" "<= func expects two number args")
            (stack-push! interp (<= num-2 num-1)))))

    (define (ge-func interp)
      (let* ((num-1 (stack-pop! interp))
             (num-2 (stack-pop! interp)))
        (if (not (and (number? num-1) (number? num-2)))
            (interp-error! interp "TypeError" ">= func expects two number args")
            (stack-push! interp (>= num-2 num-1)))))

    
    (define comp-func-dict
      `(("is" . ,is-func)
        ("equal" . ,equal-func)
        ("=" . ,eq-number-func)
        ("/=" . ,ne-number-func)
        ("<" . ,lt-func)
        (">" . ,gt-func)
        ("<=" . ,le-func)
        (">=" . ,ge-func)))))
