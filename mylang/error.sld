(define-library (mylang error)
  (export
   make-frame frame? frame-file frame-line frame-name
   make-raised raised? raised-value raised-trace
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

    (define-record-type <raised>
      (make-raised value trace)
      raised?
      (value raised-value)
      (trace raised-trace))

    (define (raise-error-value! name message line);lineがわからないparser lexer用
      (raise (make-raised
              (make-error-value name message)
              (list (make-frame #f line #f)))));#fになってるのはinterp側に任せるから
      
    (define (raised-unresolve? r)
      (not (frame-file (car (raised-trace r)))));一番手前のファイルが未完成かどうか

    (define (raised-resolve r file name callers)
      (let ((inner (car (raised-trace r))));解決したいエラーのtrace（退避）
        (make-raised
         (raised-value r)
         (cons (make-frame file (frame-line inner) name) callers))))))
