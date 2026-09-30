(define-library (mylang error)
  (export
   make-frame
   frame?
   frame-file
   frame-line
   frame-name
   make-mylang-error mylang-error?
   mylang-error-name mylang-error-message
   mylang-error-trace
   raise-mylang-error!
   mylang-error-unresolve?
   mylang-error-resolve)
  (import (scheme base))
  (begin
    (define-record-type <frame>
      (make-frame file line name)
      frame?
      (file frame-file);hoge/hogehoge/kanikani.r2pnl
      (line frame-line);4
      (name frame-name));<toplevel>
    
    (define-record-type <mylang-error>;line fileは廃止して、frameに全部持たせる
      (make-mylang-error error-name message trace)
      mylang-error?
      (error-name mylang-error-name)
      (message mylang-error-message)
      (trace mylang-error-trace))
    
    (define (raise-mylang-error! name message line);lineがわからないparser lexer用
      (raise (make-mylang-error name message (list (make-frame #f line #f)))));#fになってるのはinterp側に任せる
    (define (mylang-error-unresolve? e)
      (not (frame-file (car (mylang-error-trace e)))));一番手前のファイルが未完成かどうか

    (define (mylang-error-resolve e file name callers)
      (let ((inner (car (mylang-error-trace e))));解決したいエラーのtrace（退避）
        (make-mylang-error
         (mylang-error-name e)
         (mylang-error-message e)
         (cons (make-frame file (frame-line inner) name) callers))))))
