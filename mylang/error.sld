(define-library (mylang error)
  (export
   current-file
   make-mylang-error mylang-error?
   mylang-error-name mylang-error-message
   mylang-error-line mylang-error-trace mylang-error-trace-set!
   mylang-error-file
   raise-mylang-error!)
  (import (scheme base))
  (begin
    (define current-file (make-parameter "<toplevel>"))
    
    (define-record-type <mylang-error>
      (make-mylang-error error-name message line file trace)
      mylang-error?
      (error-name mylang-error-name)
      (message mylang-error-message)
      (line mylang-error-line)
      (file mylang-error-file)
      (trace mylang-error-trace mylang-error-trace-set!))
    
    
    (define (raise-mylang-error! name message line)
      (raise (make-mylang-error name message line (current-file) '())))));グローバルのcurrentを
