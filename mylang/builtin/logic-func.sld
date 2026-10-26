(define-library (mylang builtin logic-func)
  (export logic-func-dict)
  (import (scheme base)
          (mylang values)
          (mylang interpreter))
  (begin
    ;全部lib側に移動してしまったため書くことがない（今度廃止かな？）
    (define logic-func-dict '())))
