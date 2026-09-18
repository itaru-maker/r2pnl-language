(define-library (mylang builtin stack-func)
  (export stack-func-dict)
  (import (scheme base)
	  (scheme write)
	  (mylang interpreter))

  (begin
    (define (dup-func interp)
      (let* ((a (stack-pop! interp)))
	(stack-push! interp a)
	(stack-push! interp a)))

    (define (swap-func interp)
      (let* ((a (stack-pop! interp))
	     (b (stack-pop! interp)))
	(stack-push! interp a)
	(stack-push! interp b)))

    (define (drop-func interp)
      (stack-pop! interp))

    (define (over-func interp)
      (let* ((a (stack-pop! interp))
             (b (stack-pop! interp)))
        (stack-push! interp b)
        (stack-push! interp a)
        (stack-push! interp b)))

    (define (rot-func interp)
      (let* ((a (stack-pop! interp))
             (b (stack-pop! interp))
             (c (stack-pop! interp)))
        (stack-push! interp b)
        (stack-push! interp a)
        (stack-push! interp c)))

    (define (nip-func interp)
      (let* ((a (stack-pop! interp))
             (b (stack-pop! interp)))
        (stack-push! interp a)))

    (define (tuck-func interp)
      (let* ((a (stack-pop! interp))
             (b (stack-pop! interp)))
        (stack-push! interp a)
        (stack-push! interp b)
        (stack-push! interp a)))

    (define (two-drop-func interp)
      (stack-pop! interp)
      (stack-pop! interp))

    ;;何もしない（ここにいていいのかな）
    (define (noop-func interp)
      '())

    (define stack-func-dict
      `(("dup" . ,dup-func)
        ("swap" . ,swap-func)
        ("drop" . ,drop-func)
        ("over" . ,over-func)
        ("rot" . ,rot-func)
        ("nip" . ,nip-func)
        ("tuck" . ,tuck-func)
        ("2drop" . ,two-drop-func)
        ("noop" . ,noop-func)))))
