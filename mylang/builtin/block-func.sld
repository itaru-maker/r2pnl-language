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
          (interp-error! interp "TypeError"  "the \"head\" func expects 1 block value."))

         ((null? (block-value-items a))
          (interp-error! interp "ValueError" "the \"tail\" func expects a not null block"))

         (else
          (stack-push! interp (make-block-value(cdr (block-value-items a))))))))

    (define (block-pack-func interp)
      (let* ((args-count (stack-pop! interp)))
        (if (number? args-count)
            (let loop ((count args-count) (acc-block '()))
              (if (zero? count)
                  (stack-push! interp (make-block-value (reverse acc-block)))
                  (loop (- count 1) (cons (cons (stack-pop! interp) (interp-token-line interp)) acc-block))))
            (interp-error! interp "TypeError" "The first arg of the \"pack\" func must be a number"))))

    (define (null-block?-func interp)
      (let* ((block (stack-pop! interp)))
        (if (block-value? block)
            (stack-push! interp (null? (block-value-items block)))
            (interp-error! interp "TypeError" "the null-block? func expects 1 block-value"))))

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


    (define (block-concat-func interp)
      (let* ((block2 (stack-pop! interp))
             (block1 (stack-pop! interp)))
        (if (not (and (block-value? block1) (block-value? block2)))
            (interp-error! interp "TypeError" "the block-concat func expects two block-value")
            (stack-push! interp (make-block-value (append (block-value-items block1) (block-value-items block2)))))))

    (define (block-length-func interp)
      (let* ((block (stack-pop! interp)))
        (if (block-value? block)
            (stack-push! interp (length (block-value-items block)))
            (interp-error! interp "TypeError" "the block-length func expects one block-value"))))

    (define (block-nth-func interp)
      (let* ((i (stack-pop! interp))
             (block (stack-pop! interp)))
        (if (or
             (not (and (block-value? block) (number? i)))
             (not (integer? i))
             (< i 0))
            (interp-error! interp "TypeError" "The block-nth func expects one block-value and one positive integer")
            (begin
              (if (<= (length (block-value-items block)) i)
                  (interp-error! interp "IndexError" "block index out of range")
                  (stack-push! interp (car (list-ref (block-value-items block) (exact i)))))))))

    (define (block-slice-func interp)
      (let* ((end (stack-pop! interp))
             (start (stack-pop! interp))
             (block (stack-pop! interp)))
        (if (not (and (number? end) (number? start) (block-value? block) (integer? end) (<= 0 end) (integer? start) (<= 0 start)))
            (interp-error! interp "TypeError" "the block-slice func expects two positive integer and a block-value")
            (if (null? (block-value-items block))
                (stack-push! interp (make-block-value '()))
                (let loop ((rest (block-value-items block)) (i 0) (acc '()))
                  (cond
                   ((null? rest) (stack-push! interp (make-block-value (reverse acc))))
                   ((>= i end) (stack-push! interp (make-block-value (reverse acc))))
                   ((>= i start) (loop (cdr rest) (+ i 1) (cons (car rest) acc)))
                   (else (loop (cdr rest) (+ i 1) acc))))))))


    (define block-func-dict
      `(("head" . ,block-head-func)
        ("tail" . ,block-tail-func)
        ("block-pack" . ,block-pack-func)
        ("null-block?" . ,null-block?-func)
        ("cons" . ,block-cons-func)
        ("block-concat" . ,block-concat-func)
        ("block-length" . , block-length-func)
        ("block-nth" . ,block-nth-func)
        ("block-slice" . ,block-slice-func)))))



