(define-library (mylang builtin lambda-func)
  (export lambda-func-dict)
  (import (scheme base)
          (scheme write)
          (mylang values)
          (mylang tokens)
          (mylang interpreter))
  (begin
    ;;lispのlambdaみたいなやつ
    (define (fn-func interp);わかりにくい名前だな
      (let*
          ((body (stack-pop! interp)))
        (if (not (block-value? body))
            (interp-error! interp "TypeError" "the \"fn\" func expects a block")
            (stack-push! interp
                         (make-lambda-value
                          body
                          (interp-env interp)
                          (interp-token-line interp)
                          (interp-file interp)
                          #f)))));名前はわからない
    
    ;;スコープを作らずに、ただblock-valueをcallableにするだけ
    (define (proc-func interp)
      (let*
          ((body (stack-pop! interp)))
        (if (not (block-value? body))
            (interp-error! interp "TypeError" "the \"proc\" func expects a block")
            (stack-push! interp
                         (make-proc-value
                          body
                          (interp-token-line interp)
                          (interp-file interp)
                          #f)))))
    
    (define lambda-func-dict ;これを返す
      `(("fn" . ,fn-func)
        ("proc" . ,proc-func)))))
