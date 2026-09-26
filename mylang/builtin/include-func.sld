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
      (let* ((path (stack-pop! interp))
             (caller-line (interp-token-line interp)))
        (cond
         ((not (string? path)) (interp-error! interp "TypeError" "the \"include\" func expects a string"))
         ((not (file-exists? path)) (interp-error! interp "IOError" (string-append "no such file:" path)))
         (else
          (guard (e
                  ((mylang-error? e)
                   (mylang-error-trace-set!
                    e
                    (cons
                     (string-append "from " (current-file) " at line " (number->string (caller-line)))
                     (mylang-error-trace e)))
                   (interp-token-line-set! interp caller-line);戻してあげる
                   (raise e)))
            (let* ((raw (lexar (read-all-text path)))
                   (types (sorting-types raw))
                   (structure (parse-paren types)))
              (execute-body interp structure)
              (interp-token-line-set! interp caller-line)))))))
    
    (define include-func-dict
      `(("include" . ,include-func)))))
