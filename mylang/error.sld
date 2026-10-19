(define-library (mylang error)
  (export
   make-frame
   frame?
   frame-file
   frame-line
   frame-name
   make-error-value error-value?
   error-value-name error-value-message
   error-value-trace
   raise-error-value!
   error-value-unresolve?
   error-value-resolve)
  (import (scheme base)
          (mylang values))
  (begin
    (define-record-type <frame>
      (make-frame file line name)
      frame?
      (file frame-file);hoge/hogehoge/kanikani.r2pnl
      (line frame-line);4
      (name frame-name));<toplevel>

    (define (raise-error-value! name message line);lineがわからないparser lexer用
      (raise (make-error-value name message (list (make-frame #f line #f)))));#fになってるのはinterp側に任せる
    (define (error-value-unresolve? e)
      (not (frame-file (car (error-value-trace e)))));一番手前のファイルが未完成かどうか

    (define (error-value-resolve e file name callers)
      (let ((inner (car (error-value-trace e))));解決したいエラーのtrace（退避）
        (make-error-value
         (error-value-name e)
         (error-value-message e)
         (cons (make-frame file (frame-line inner) name) callers))))))
