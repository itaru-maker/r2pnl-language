(define-library (r2pnl builtin include-func)
  (export include-func-dict
          include-file!)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (r2pnl values)
          (r2pnl env)
          (r2pnl tokens)
          (r2pnl error)
          (r2pnl lexer)
          (r2pnl parser)
          (r2pnl interpreter))
  (begin
    (define (read-all-text file-path)
      (call-with-input-file file-path
        (lambda (port)
          (let loop ((acc '()))
            (let ((chr (read-char port)))
              (if (eof-object? chr)
                  (list->string (reverse acc))
                  (loop (cons chr acc))))))))

    (define (include-file! interp path)
        (cond
         ((not (file-exists? path)) (interp-error! interp "IOError" (string-append "no such file:" path)))
         (else
          (let* ((structure (parse-paren (sorting-types (lexer (read-all-text path)))))
                 (caller-line (cons (current-frame interp) (interp-call-stack interp))))
            (call-with-frame interp path 1 "<toplevel>"
                             (lambda () (execute-body interp structure)))))))

    (define (include-func interp)
      (let* ((path (stack-pop! interp)))
        (if (not (string? path))
            (interp-error! interp "TypeError" "the \"include\" func expects a string")
            (include-file! interp path))))



    (define include-func-dict
      `(("include" . ,include-func)))))
