;;#!/usr/bin/env -S gsi -:r7rs,search=. ;gambit用
#|
-================実行方法===============-
お使いの処理系に応じて使い分けてください！
gauche （エラーコードがわかりやすい）
gosh -r7 -I. main.scm 実行したいファイル
chibi（r7rsに忠実）
chibi-scheme main.scm 実行したいファイル
kawa
kawa -Dkawa.import.path="./*.sld" --r7rs main.scm 実行したいファイル
sash
sash -r7 -L . main.scm 実行したいファイル
guile（速い！）
guile --r7rs -L . main.scm 実行したいファイル
gambit（一応動くけど）
このファイルの先頭のコメントを外す
インタプリタ->gsi -:r7rs,search=. main.scm 実行したいファイル
コンパイラ->gsc -:r7rs,search=. -exe -nopreload main.scm からの./main
で動くはず!
chicken
むりだった！(r7rs-eggがsldファイルに非対応のため)
cyclone
むりだった！(apple silicon macとの相性が悪かった)
loko
むりだった！(たぶんarm非対応)
-=======================================-
|#

(import (scheme base)
        (scheme write)
        (scheme file)
        (scheme process-context)
        (r2pnl values)
        (r2pnl tokens)
        (r2pnl env)
        (r2pnl lexer)
        (r2pnl parser)
        (r2pnl error)
        (r2pnl interpreter)
        (r2pnl builtin prelude)
        (r2pnl builtin include-func))

(define r2pnl (make-interp all-builtins));インスタンス化

(define (fatal-error! name message)
  (newline)
  (display (string-append name ":" message) (current-error-port))
  (newline (current-error-port))
  (exit 1))

(define (last-item lst)
  (let loop ((current (car lst))
             (rest (cdr lst)))
    (if (null? rest)
        current
        (loop (car rest) (cdr rest)))))

(define (read-all-text file-path)
  (call-with-input-file file-path
    (lambda (port)
      (let loop ((acc '()))
        (let ((chr (read-char port)))
          (if (eof-object? chr)
              (list->string (reverse acc))
              (loop (cons chr acc))))))))

(define (get-code file-path)
  (if (file-exists? file-path)
      (call-with-input-file file-path
        (lambda (port)
          (let ((content (read-all-text file-path)))
            (if (eof-object? content)
                ""
                (string-copy content)))))
        (fatal-error! "IOError"(string-append "can't open file '" file-path "'"))))

;;V 将来的にはmain.scmをエントリポイントにして、その一つ後を受け取るようにしたい
(define (target-file-path)
  (let ((user-command (command-line)))
    (last-item user-command)))

(define r2pnl-code
  (get-code (target-file-path)))

(interp-run r2pnl (get-code "lib/std.r2pnl") "lib/std.r2pnl")
;;ここで実行
(interp-run r2pnl r2pnl-code (target-file-path))

(if (not (null? (interp-stack r2pnl)));安全確認　（これはいらないっていう人もいるかも）
    (begin
      (display "WARNING:stack is not empty at end of program\n" (current-error-port))
      (display "==top===\n" (current-error-port))
      (for-each (lambda (v)
                  (display (value->write-string v)(current-error-port))
                  (newline (current-error-port)))
                (interp-stack r2pnl))
      (display "=bottom=" (current-error-port))
      (exit 1)))
