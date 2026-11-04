(define-library (r2pnl builtin control-func)
  (export control-func-dict)
  (import (scheme base)
          (r2pnl values)
          (r2pnl error)
          (r2pnl env)
          (r2pnl tokens)
          (r2pnl interpreter)
          (r2pnl parser))

  (begin
    (define (call-func interp)
      (let ((v (stack-pop! interp)))
        (if (not (or (builtin-func? v) (block-value? v)))
            (interp-error! interp
                           "TypeError"
                           (string-append
                            "the \"call\" func expects a callable (builtin or block), but got "
                            (value->write-string v)))
            (apply-callable! interp v))))

    (define (if-func interp)
      (let*
          ((false-then (stack-pop! interp))
           (true-then (stack-pop! interp))
           (condition (stack-pop! interp)))
        (if (not (and (block-value? true-then) (block-value? false-then)))
            (interp-error! interp "TypeError" (string-append "the \"if\" func expects one any value and two block, but got "
                                                             (value->write-string true-then)
                                                             " and "
                                                             (value->write-string false-then)))
        (if (or
             (eq? #f condition)
             (nil-value? condition))
            (exec-block false-then interp)
            (exec-block true-then interp)))))


    (define (while-func interp)
      (let*
          ((body (stack-pop! interp))
           (cond-block (stack-pop! interp)))
        (if (or (not (block-value? cond-block)) (not (block-value? body)))
            (interp-error! interp "TypeError" (string-append
                                               "the \"while\" func expects two block, but got "
                                               (value->write-string body)
                                               " and "
                                               (value->write-string cond-block)))
            (let loop ();このループを回す
              (let ((before-len (length (interp-stack interp))))
                (exec-block cond-block interp)
                (if (not (= (+ before-len 1) (length (interp-stack interp))))
                    (interp-error! interp "StackError" "the condition block of the \"while\" func must push exactly one value")
                    (let ((now-cond (stack-pop! interp)))
                      (if (or (eq? the-nil now-cond) (eq? #f now-cond))
                          '()                 ;終わり。何もしない
                          (begin
                            (exec-block body interp)
                            (loop))))))))))
      
    (define (repeat-func interp)
      (let*
          ((repeat-body (stack-pop! interp))
           (repeat-count (stack-pop! interp)))
        (if (not (and
                  (number? repeat-count)
                  (< 0 repeat-count)
                  (block-value? repeat-body)))
            (interp-error! interp "TypeError" (string-append "the \"repeat\" func expects a positive number and a block value, but got "
                                                 (value->write-string repeat-count)
                                                 " and "
                                                 (value->write-string repeat-body)))
            (let loop ((current-num 0) (limit (truncate repeat-count)))
              (if (<= limit current-num)
                  '()
                  (begin
                   (exec-block repeat-body interp)
                   (loop (+ current-num 1) limit)))))))

    (define (cond-func interp) ;malformedチェックが必要
      (let* ((cond-block (stack-pop! interp)))
        (if (not (block-value? cond-block))
            (interp-error! interp "TypeError" (string-append  "the \"cond\" func expects one block value, but got"
                                                 (value->write-string cond-block)))
            (let ((block-items (block-value-items cond-block)))
              (if (odd? (length block-items))
                  (interp-error! interp "ValueError" "the \"cond\" func expects one block-value of even elements.")
                  (let loop ((rest block-items))
                    (if (null? rest)
                        (interp-error! interp "ValueError" "No clause in the cond expression was executed. Did you forget { #true } { ... }?")
                        (let ((condition (caar rest))
                              (body  (caar (cdr rest))))
                          (begin
                            (exec-block condition interp)
                            (let ((result (stack-pop! interp)))
                              (if (or (eq? #f result) (eq? the-nil result))
                                  (loop (cddr rest))
                                  (exec-block body interp))))))))))))


    (define (try-func interp)
      (let* ((handler (stack-pop! interp))
             (body (stack-pop! interp)))
        (if (not (and (block-value? handler) (block-value? body)))
            (interp-error! interp "TypeError" "the \"try\" func expects two blocks")
            (let ((saved-stack (interp-stack interp));元の環境を汚さないためにコピーを作っておく
                  (saved-env (env-snapshot (interp-env interp))))
              (guard
                  (e
                   ((raised? e)
                    (interp-stack-set! interp saved-stack)
                    (env-restore! (interp-env interp) saved-env)
                    (stack-push! interp (raised-value e))
                    (exec-block handler interp)))
                (exec-block body interp))))))
    
    (define (raise-func interp)
      (interp-throw! interp (stack-pop! interp)))

    (define (scope-func interp)
      (let* ((body (stack-pop! interp)))
        (if (not (block-value? body))
            (interp-error! interp "TypeError" "the \"scope\" func expects a block")
            (let ((snap (env-snapshot (interp-env interp))))
              (dynamic-wind ;絶対全部やる
                (lambda () '())
                (lambda () (exec-block body interp))
                (lambda () (env-restore! (interp-env interp) snap)))))))

    (define control-func-dict
      `(("call" . ,call-func)
        ("if" . ,if-func)
        ("while" . ,while-func)
        ("repeat" . ,repeat-func)
        ("cond" . , cond-func)
        ("try" . ,try-func)
        ("raise" . ,raise-func)
        ("scope" . ,scope-func)))))


