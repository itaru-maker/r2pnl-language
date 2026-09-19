(define-library (mylang parser)
  (export parse-one-token
	  sorting-types
	  parse-paren)
  (import (scheme base)
	  (scheme write)
	  (mylang values)
	  (mylang tokens)
	  (mylang error))
  (begin
    (define (remove-esc str line)
      (let ((len (string-length str)))
        (let loop ((i 0) (acc '()))
          (if (<= len i)
              (list->string (reverse acc))
              (let ((current (string-ref str i)))
                (if (and (char=? current #\\) (< (+ i 1) len));esc?&nextは取れる？（取れないなら自動的に取らずに進める）
                    (let ((next (string-ref str (+ i 1))))
                      (case next
                        ((#\') (loop (+ i 2) (cons #\" acc)));lexar複雑化したくなかった（ゆるして）
                        ((#\n) (loop (+ i 2) (cons #\newline acc)))
                        ((#\\) (loop (+ i 2) (cons #\\ acc)))
                        ((#\t) (loop (+ i 2) (cons #\tab acc)))
                        ((#\b) (loop (+ i 2) (cons #\backspace acc)))
                        ((#\a) (loop (+ i 2) (cons #\alarm acc)))
                        ((#\newline) (loop (+ i 2) acc))
                        (else (raise-mylang-error!
                               "ParseError"
                               (string-append
                                "unknown escape:"
                                (string next))
                               line))))
                    (loop (+ i 1) (cons current acc))))))))
    
    (define (parse-one-token item line)
      ;;一つのtokenを受け取って、変換して返す
      (let ((token-len (string-length item))
	    (item-line line));parse-error用
	(cond
	 ;;数値
	 ((string->number item)
	  (string->number item))
	 
	 ;;文字列
	 ((and (< 1 token-len)
	       (char=? (string-ref item 0) #\")
	       (char=? (string-ref item (- token-len 1)) #\"))
	  (remove-esc (substring item 1 (- token-len 1)) line))

	 ;;真偽地&nil
	 ((string=? item "#true") #t) ;true
	 ((string=? item "#false") #f) ;false
	 ((string=? item "#nil") the-nil) ;nil

	 ((string=? item "'" )
	  (raise-mylang-error! "ParseError" "value missing after single quote." item-line))
	 ;;lazy
	 ((char=? (string-ref item 0) #\')
	  (make-lazy-value (substring item 1 token-len)))


         ;;tokenとか 
	 ((string=? item "{") the-l-paren)
	 ((string=? item "}") the-r-paren)

	 ;;symbol
	 (else (make-symbol-value item)))))


    (define (sorting-types str-tokens)
      ;;parse-one-token　を全てのtokenについて行う
      (map (lambda (pair)
	     (cons (parse-one-token (car pair) (cdr pair));もうちょっと簡潔にかける気が
		   (cdr pair)))
	   str-tokens))


    (define (parse-paren types)
      ;;blockを構造化
      (let loop ((rest types)
		 (stack (list (cons (make-block-value '()) #f)))) ;外側の受け皿 (逆向き) 行番号はなし
	(cond
	 ((null? rest)
	  (if (null? (cdr stack));スタックに2以上あるなら、とじかっこが少ない（解決していない）
	      (reverse (block-value-items (car (car stack))))
	      (raise-mylang-error! "ParseError" "unclosed paren" (cdr (car stack)))))
     
	 ((l-paren? (car (car rest)));ペアの中の値をみる
	  (loop (cdr rest)
		(cons (cons (make-block-value '()) (cdr (car rest))) stack)))
	 
	 ((r-paren? (car (car rest)))
	  (if (null? (cdr stack)) ;すでにblockが解決している場合
	      (raise-mylang-error! "ParseError" "extra closing paren" (cdr (car rest)))
	      (let ((new-block (car (car stack)))
		    (rest-stack(cdr stack))
		    (line (cdr (car rest)))) ;自身((car (car rest))) が持ってるline
		(block-value-items-set! new-block (reverse (block-value-items new-block))) ; ここでも裏返す
		(block-value-append! (car (car rest-stack)) (cons new-block line))
		(loop (cdr rest) rest-stack))))
	 (else
	  (block-value-append! (car (car stack))(car rest))
	  (loop (cdr rest) stack)))))))

