(define-library (r2pnl builtin prelude)
  (export all-builtins)
  (import (scheme base)
	  (scheme write)
	  (r2pnl builtin math-func)
	  (r2pnl builtin io-func)
	  (r2pnl builtin var-func)
	  (r2pnl builtin stack-func)
	  (r2pnl builtin control-func)
          (r2pnl builtin seq-func)
          (r2pnl builtin comp-func)
          (r2pnl builtin block-func)
          (r2pnl builtin value-func)
          (r2pnl builtin list-func)
          (r2pnl builtin higher-order-func)
          (r2pnl builtin string-func)
          (r2pnl builtin convert-func)
          (r2pnl builtin file-func)
          (r2pnl builtin include-func)
          (r2pnl builtin plist-func)
          (r2pnl builtin error-func))

  (begin
    (define all-builtins
      (append
       math-func-dict
       io-func-dict
       var-func-dict
       stack-func-dict
       control-func-dict
       seq-func-dict
       comp-func-dict
       block-func-dict
       value-func-dict
       list-func-dict
       higher-order-func-dict
       string-func-dict
       convert-func-dict
       file-func-dict
       include-func-dict
       plist-func-dict
       error-func-dict))))
