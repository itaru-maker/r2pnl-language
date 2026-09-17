#|実行方法：お使いの処理系に応じて使い分けてください！
gauche （エラーコードがわかりやすい）
gosh -r7 -I. main.scm
chibi（r7rsに忠実）
chibi-scheme main.scm
kawa
kawa -Dkawa.import.path="./*.sld" --r7rs main.scm
sash
sash -r7 -L . main.scm
guile（速い！）
guile --r7rs -L . main.scm
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

(import (scheme base)
        (scheme file))

(define (get-code arg-path)
  (if (file-exists? arg-path)
      (call-with-input-file arg-path
        (lambda (port)
          (let ((content (read-string 10000 port)))
            (if (eof-object? content)
                ""
                (string-copy content)))))
      (begin
        (display (string-append "-====ERROR====-\ncan't open file '" arg-path "'"));少しやり方が汚い可能性あり
        "")))

(define mylang-code
  (get-code "test.r2pnl"))

(interp-run mylang mylang-code);実行する
(newline)


