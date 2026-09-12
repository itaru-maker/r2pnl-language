(define-library (mylang error)
  (export
   make-mylang-error mylang-error?
   mylang-error-name mylang-error-message
   mylang-error-line mylang-error-trace mylang-error-trace-set!
   raise-mylang-error!)
  (import (scheme base))
  (begin
    ;;ここに処理を
    (define-record-type <mylang-error>
      (make-mylang-error error-name message line trace)
      mylang-error?
      (error-name mylang-error-name)
      (message mylang-error-message)
      (line mylang-error-line)
      (trace mylang-error-trace mylang-error-trace-set!))
    
    
    (define (raise-mylang-error! name message line)
      (raise (make-mylang-error name message line '())))))
