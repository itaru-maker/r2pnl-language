(define-library (r2pnl builtin file-func)
  (export file-func-dict)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (r2pnl values)
          (r2pnl tokens)
          (r2pnl interpreter)
          (r2pnl error))

  (begin
    (define (file-exists?-func interp)
      (let* ((name (stack-pop! interp)))
        (if (string? name)
            (stack-push! interp (file-exists? name))
            (interp-error! interp "TypeError" "the \"file-exists?\" func expects a string"))))

    (define file-func-dict
      `(("file-exists?" . ,file-exists?-func)))))
