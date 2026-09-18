#|実行方法：お使いの処理系に応じて使い分けてください！
gauche （エラーコードがわかりやすい）
gosh -r7 -I. main.scm 実行したいファイル.r2pnl
chibi（r7rsに忠実）
chibi-scheme main.scm 実行したいファイル.r2pnl
kawa
kawa -Dkawa.import.path="./*.sld" --r7rs main.scm 実行したいファイル.r2pnln
sash
sash -r7 -L . main.scm 実行したいファイル.r2pnl
guile（速い！）
guile --r7rs -L . main.scm 実行したいファイル.r2pnl
chicken
 むりだった！(r7rs-eggがsldファイルに非対応のため)
cyclone
むりだった！(apple silicon macとの相性が悪かった)
==================
|#

(import (scheme base)
        (scheme write)
        (scheme file)
        (scheme process-context)
        (mylang values)
        (mylang tokens)
        (mylang env)
        (mylang lexar)
        (mylang parser)
        (mylang error)
        (mylang interpreter)
        (mylang builtin prelude))

(define mylang (make-interp all-builtins));インスタンス化

(define (fatal-error! message)
  (newline)
  (display "-====ERROR====-")
  (newline)
  (display message)
  (newline)
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
        (fatal-error! (string-append "can't open file '" file-path "'"))))

;;V 将来的にはmain.scmをエントリポイントにして、その一つ後を受け取るようにする
(define (target-file-path)
  (let ((user-command (command-line)))
    (last-item user-command)))

(define mylang-code
  (get-code (target-file-path)))

(interp-run mylang mylang-code)
(newline)


