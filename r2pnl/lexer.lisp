(defvar *cmt-char* #\!)

(defun flash-chars (chars tokens line)
          (if (null chars)
              tokens
              (cons (cons (coerce (reverse chars) 'string) line) tokens)))

(defun lexer (plain-code)
    (let ((code-len (length plain-code)))
      (let ((i 0)
            (chars '())
            (in-str? nil)
            (cmt-depth 0)
            (in-line-cmt? nil)
            (line 1)
            (tokens '()))
        
        (loop while (/= i code-len) do
          (let ((cur (char plain-code i)))
            (incf i)
            (cond
              ;;ブロックコメント
              ((< 0 cmt-depth)
               (cond
                 ((char= #\newline cur)
                  (incf line))
                 
                 ((char= #\( cur)
                  (incf cmt-depth))

                 ((char= #\) cur)
                  (decf cmt-depth))))

              ;;行コメント
              (in-line-cmt?
               (when (char= #\newline cur)
                 (incf line)
                 (setq in-line-cmt? nil)))

              ((char= #\" cur)
               (setq chars (cons cur chars)))

              ((and (not in-str?) (not in-line-cmt?) (char= #\( cur))
               (setq tokens (flash-chars chars tokens line)))

              ((and (not in-str?) (not in-line-cmt?) (char= #\) cur))
               (error "とじカッコが多すぎる!(簡易)"))

              ((and (not in-str?) (= cmt-depth 0) (char= *cmt-char* cur))
               (when (char= cur #\newline)
                 (incf line))
               (setq tokens (flash-chars chars tokens line)
                     chars '()
                     in-line-cmt? t))

              ((char= #\newline cur)
               (incf line)
               (setq tokens (flash-chars chars tokens line)
                     chars '()))

              ((or (member cur '(#\tab #\space)))
               (setq tokens (flash-chars chars tokens line)
                     chars '()))

              (t
               (setq chars (cons cur chars))))))

        (if (> cmt-depth 0)
            (error "コメントが閉じてないよ!!")
            (reverse (flash-chars chars tokens line))))))
        

(print (lexer "
'pi { 
    1 0.0 !i total 
    rot {
        1 2 pick / +
        { 2 + } dip
        1 2 pick / - 
        { 2 + } dip
    } repeat
    nip 4 *
} fn

100000 pi print

"))
