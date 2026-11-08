(defun parse-number (str)
  (when (and (plusp (length str))
             (every (lambda (c) (find c "0123456789+-.e")) str))
    (let* ((*read-eval* nil);安全用のおまじない
           (*read-default-float-format* 'double-float)
           (*package* (find-package :keyword)));おまじない2
      (let ((v (ignore-errors (read-from-string str))))
        (and (numberp v) v)))))


(defun parse-one-token (item line)
  (let ((token-len (length item))
        (numberp (parse-number item)))
    (cond
      ;;数値
      (numberp numberp)

      ;;文字列
      ((and (< 1 token-len)
            (char= #\"
                   (char item 0)
                   (char item (1- token-len))))
       (subseq item 1 (1- token-len)))

      ((string= item "#true") *the-true*)
      ((string= item "#false") *the-false*)
      ((string= item "#nil") *the-nil*)

      ((string= item "'" )
       (error "value missing after single quote."))

      ((char= #\' (char item 0))
       (make-quote-value (subseq item 1 token-len)))

      ((string= item "{") the-lparen)
      ((string= item "}") the-rparen)

      (t (make-symbol-value item)))))

(defun sorting-types (tokens)
  (map #'(lambda (pair)
           (cons (parse-one-token (car pair) (cdr pair))
                 (cdr pair)))
       tokens))

(defun parse-paren (types)
  (let ((rest types)
        (stack (list (cons (make-block-value '() nil)))))
    (loop while (not (null rest)) do
          (cond
           ((lparen-p (car stack))
            (setq stack (cons (cons (make-block-value '()) (cdr (car rest))) stack))
            (setq rest (cdr rest)))

           ((rparen-p (caar rest))
            (if (null (cdr stack))
                (error "とじカッコが多すぎる！")
              (let ((new-block (caar stack))
                    (rest-stack (cdr stack))
                    (line (cdar rest)))
                (block-value-item-set! new-block (reverse (block-value-items new-block)))
                (block-value-append! (caar rest-stack) (cons new-block line))))
            (setq rest (cdr rest)))

           (t
            (block-value-append! (caar stack) (car rest))
            (setq rest (cdr rest))))))


