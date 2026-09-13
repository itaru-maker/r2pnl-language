(define-library (mylang builtin prelude)
  (export all-builtins)
  (import (scheme base)
	  (scheme write)
	  (mylang builtin math-func)
	  (mylang builtin io-func)
	  (mylang builtin var-func)
	  (mylang builtin stack-func)
	  (mylang builtin control-func)
          (mylang builtin lambda-func)
          (mylang builtin comp-func)
          (mylang builtin block-func)
          (mylang builtin value-func)
          (mylang builtin list-func)
;;          (mylang builtin higher-order-func)
          (mylang builtin string-func)
          (mylang builtin logic-func)
          (mylang builtin convert-func)
          )

  (begin
    (define all-builtins
      (append
       math-func-dict
       io-func-dict
       var-func-dict
       stack-func-dict
       control-func-dict
       lambda-func-dict
       comp-func-dict
       block-func-dict
       value-func-dict
       list-func-dict
;;       higher-order-func-dict
       string-func-dict
       logic-func-dict
       convert-func-dict
       ))))
