(define-library (mylang builtin control-func)
  (export control-func-dict)
  (import (scheme base)
          (mylang values)
          (mylang error)
          (mylang env)
          (mylang tokens)
          (mylang interpreter)
          (mylang parser))

  (begin
    (define (do-func interp)
      ;;一つのブロックを実行する
      (let ((block (stack-pop! interp)))
        (if (not (block-value? block))
            (interp-error! interp
                           "TypeError"
                           (string-append
                            "the \"exec\" func is expects a block, but got "
                            (value->write-string block)))
            (exec-block block interp))))

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

    (define (when-func interp)
      (let* ((true-then (stack-pop! interp))
            (condition (stack-pop! interp)))
        (if (block-value? true-then)
            (if (or (eq? #f condition) (eq? the-nil condition))
               '();何もしない
               (exec-block true-then interp))
            (interp-error!
             interp
             "TypeError"
             (string-append "the \"when\" func expects a any value and a block, but got "
                            (value->write-string true-then))))))

    

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
            (let loop ()                ;このループを回す
              (exec-block cond-block interp)
              (let ((now-cond (stack-pop! interp)))
                (if (or (eq? the-nil now-cond) (eq? #f now-cond))
                    '()                 ;終わり。何もしない
                    (begin
                      (exec-block body interp)
                      (loop))))))))

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

    (define (for-func interp)
      (let* ((body (stack-pop! interp))
             (end (stack-pop! interp))
             (start (stack-pop! interp))
             (label (stack-pop! interp)))
        (if (not (and
                  (block-value? body)
                  (number? end)
                  (number? start)
                  (symbol-value? label)))
            (interp-error! interp "TypeError" "the \"for\" func expects a symbol, two numbers and a block")
            (let*
                ((caller-env (interp-env interp));一旦interp側のenvを退避
                 (loop-env (make-env caller-env)));退避させたやつを親にしたenvを作る
              (let ((var-name (symbol-value-token label)))
                (env-define loop-env var-name start)
                (interp-env-set! interp loop-env)
                (let loop ((current-num start))
                  (if (< end current-num)
                      (interp-env-set! interp caller-env);戻す
                      (begin
                        (env-set! loop-env var-name current-num)
                        (exec-block body interp)
                        (loop (+ current-num 1))
                        (interp-env-set! interp caller-env)))))))))


    (define (try-func interp)
      (let* ((handler (stack-pop! interp))
             (body (stack-pop! interp)))
        (if (not (and (block-value? handler) (block-value? body)))
            (interp-error! interp "TypeError" "the \"try\" func expects two blocks")
            (guard
                (e
                 ((mylang-error? e)
                  (stack-push! interp (mylang-error-name e))
                  (stack-push! interp (mylang-error-message e))
                  (exec-block handler interp)))
              (exec-block body interp)))))

    (define (raise-func interp)
      (let* ((msg (stack-pop! interp))
             (name (stack-pop! interp)))
        (if (not (and (string? msg) (string? name)))
            (interp-error! interp "TypeError" "the \"raise\" func expects two strings")
            (raise-mylang-error! name msg (interp-token-line interp)))))


    (define control-func-dict
      `(("do" . ,do-func)
        ("if" . ,if-func)
        ("when" . ,when-func)
        ("while" . ,while-func)
        ("repeat" . ,repeat-func)
        ("cond" . , cond-func)
        ("for" . ,for-func)
        ("try" . ,try-func)
        ("raise" . ,raise-func)))))


