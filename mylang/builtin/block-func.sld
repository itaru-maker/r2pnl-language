(define-library (mylang builtin block-func)
  (export
   block-func-dict)
  
  (import (scheme base)
          (scheme write)
          (mylang values)
          (mylang parser)
          (mylang interpreter))
  (begin
    (define (block-head-func interp)
      (let* ((a (stack-pop! interp)))
        (cond
         ((not (block-value? a))
          (interp-error! interp "TypeError"  "the \"head\" func expects 1 block value."))

         ((null? (block-value-items a))
          (interp-error! interp "ValueError" "the \"head\" func expects a not null block"))

         (else
          (stack-push! interp (car (car (block-value-items a))))))))
    
    (define (block-tail-func interp)
      (let* ((a (stack-pop! interp)))
        (cond
         ((not (block-value? a))
          (interp-error! interp "TypeError"  "the \"tail\" func expects 1 block value."))

         ((null? (block-value-items a))
          (interp-error! interp "ValueError" "the \"tail\" func expects a not null block"))

         (else
          (stack-push! interp (make-block-value(cdr (block-value-items a))))))))

    (define (block-pack-func interp)
      (let* ((args-count (stack-pop! interp)))
        (if (and (number? args-count) (integer? args-count) (<= 0 args-count)) 
            (let loop ((count args-count) (acc-block '()))
              (if (zero? count)
                  (stack-push! interp (make-block-value (reverse acc-block)))
                  (loop (- count 1) (cons (cons (stack-pop! interp) (interp-token-line interp)) acc-block))))
            (interp-error! interp "TypeError" "The first arg of the \"pack\" func must be a positive integer"))))

    (define (block-cons-func interp)
      (let* ((block (stack-pop! interp))
             (value (stack-pop! interp)))
        (if (block-value? block)
            (stack-push! interp
                         (make-block-value
                          (cons (cons value (interp-token-line interp))
                          (block-value-items block))))
            (interp-error!
             interp
             "TypeError" "The second arg of the \"cons\" func must be a block"))))

    (define (block-slice-func interp)
      (let* ((end (stack-pop! interp))
             (start (stack-pop! interp))
             (block (stack-pop! interp)))
        (if (not (and (number? end) (number? start) (block-value? block) (integer? end) (<= 0 end) (integer? start) (<= 0 start)))
            (interp-error! interp "TypeError" "the \"slice\" func expects two positive integer and a block-value")
            (if (null? (block-value-items block))
                (stack-push! interp (make-block-value '()))
                (let loop ((rest (block-value-items block)) (i 0) (acc '()))
                  (cond
                   ((null? rest) (stack-push! interp (make-block-value (reverse acc))))
                   ((>= i end) (stack-push! interp (make-block-value (reverse acc))))
                   ((>= i start) (loop (cdr rest) (+ i 1) (cons (car rest) acc)))
                   (else (loop (cdr rest) (+ i 1) acc))))))))

    (define (range-func interp)
      (let* ((end (stack-pop! interp))
             (start (stack-pop! interp)))
        (if (not (and (number? end) (number? start) (integer? end) (integer? start)))
            (interp-error! interp "TypeError" "the \"range\" func expects two positive integer.")
            (let loop ((current start) (acc '()))
              (if (>= current end)
                  (stack-push! interp (make-block-value (reverse acc)))
                  (loop (+ current 1) (cons (cons current (interp-token-line interp)) acc)))))))

    (define (set-nth-func interp)
      (let* ((v (stack-pop! interp))
             (i (stack-pop! interp))
             (block (stack-pop! interp)))
        (if (not (and (block-value? block) (number? i) (integer? i) (<= 0 i)))
            (interp-error! interp "TypeError" "the \"set-nth\" func expects a block, a positive integer, and a any value")
            (let ((items (block-value-items block)))
              (if (<= (length items) i)
                  (interp-error! interp "IndexError" "the \"block index out of range (the \"set-nth\" func)")
                  (let loop ((rest items) (idx 0) (acc '()));accは前側のいじらなくてもいいところ
                    (if (>= idx i)
                        (stack-push! interp
                                    (make-block-value
                                     (append
                                      (reverse acc)
                                      (cons (cons v (interp-token-line interp)) (cdr rest)))))
                        (loop (cdr rest) (+ idx 1) (cons (car rest) acc)))))))))
    
    (define block-func-dict
      `(("head" . ,block-head-func)
        ("tail" . ,block-tail-func)
        ("pack" . ,block-pack-func)
        ("cons" . ,block-cons-func)
        ("slice" . ,block-slice-func)
        ("range" . ,range-func)
        ("set-nth" . ,set-nth-func)))))



