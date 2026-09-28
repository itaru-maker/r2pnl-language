(define-library (mylang builtin include-func)
  (export include-func-dict)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (mylang values)
          (mylang tokens)
          (mylang error)
          (mylang lexar)
          (mylang parser)
          (mylang interpreter))
  (begin
    (define (read-all-text file-path)
      (call-with-input-file file-path
        (lambda (port)
          (let loop ((acc '()))
            (let ((chr (read-char port)))
              (if (eof-object? chr)
                  (list->string (reverse acc))
                  (loop (cons chr acc))))))))
    
    (define (include-func interp)
      (let* ((path (stack-pop! interp)))
        (cond
         ((not (string? path)) (interp-error! interp "TypeError" "the \"include\" func expects a string"))
         ((not (file-exists? path)) (interp-error! interp "IOError" (string-append "no such file:" path)))
         (else
          (let* ((structure (parse-paren (sorting-types (lexar (read-all-text path)))))
                 (caller-line (cons (current-frame interp) (interp-call-stack interp))))
            (call-with-frame interp path 1 "<toplevel>"
                             (lambda () (execute-body interp structure))))))))
    
    (define include-func-dict
      `(("include" . ,include-func)))))
