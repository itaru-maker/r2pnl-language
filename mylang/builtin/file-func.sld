(define-library (mylang builtin file-func)
  (export file-func-dict)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (mylang values)
          (mylang tokens)
          (mylang interpreter)
          (mylang error))

  (begin
    (define (file-exists?-func interp)
      (let* ((name (stack-pop! interp)))
        (if (string? name)
            (stack-pop! interp (file-exists? name))
            (interp-error! interp "TypeError" "the \"file-exists?\" func expects a string"))))

    (define file-func-dict
      `(("file-exists?" . ,file-exists?-func)))))
