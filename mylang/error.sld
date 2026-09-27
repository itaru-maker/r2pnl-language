(define-library (mylang error)
  (export
   current-file
   make-frame
   frame-file
   frame-line
   make-mylang-error mylang-error?
   mylang-error-name mylang-error-message
   mylang-error-line 
   mylang-error-file
   mylang-error-call-stack
   raise-mylang-error!)
  (import (scheme base))
  (begin
    (define current-file (make-parameter "<toplevel>"))

    (define-record-type <frame>
      (make-frame file line)
      frame?
      (file frame-file)
      (line frame-line))
    (define-record-type <mylang-error>
      (make-mylang-error error-name message line file call-stack)
      mylang-error?
      (error-name mylang-error-name)
      (message mylang-error-message)
      (line mylang-error-line)
      (file mylang-error-file)
      (call-stack mylang-error-call-stack))
    
    
    (define (raise-mylang-error! name message line call-stack)
      (raise (make-mylang-error name message line (current-file) call-stack)))));グローバルのcurrentを
