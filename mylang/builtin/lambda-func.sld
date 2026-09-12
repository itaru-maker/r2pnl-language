(define-library (mylang builtin lambda-func)
  (export lambda-func-dict)
  (import (scheme base)
          (scheme write)
          (mylang values)
          (mylang tokens)
          (mylang interpreter))
  (begin
    (define (fn-func interp);わかりにくい名前だな
      ;;lispのlambdaみたいなやつ
      (let*
          ((body (stack-pop! interp)))

      (if (not (block-value? body))
          (interp-error! interp "TypeError" "the \"func\" func expects one block-value args"))

      (stack-push! interp
        (make-lambda-value
         body
         (interp-env interp);現在のenv
         (interp-token-line interp)))));現在のline
    
    (define lambda-func-dict ;これを変えす
      `(("fn" . ,fn-func)))))
