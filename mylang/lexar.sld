(define-library (mylang lexar)
  (export lexar)
  (import (scheme base)
          (mylang error))
  (begin

    (define cmt-chr #\!)
    
    ;; plain-codeを受け取り、((token . line) (token . line)) の形で返す
    (define (lexar plain-code)
      (let ((code-len (string-length plain-code)))
	(let loop ((i 0) ;何文字目
		   (chars '()) ; 現在のtoken の、文字ごとのリスト (逆向き)
		   (in-str? #f)
                   (comment-depth 0)
                   (in-line-cmt? #f)
		   (line 0) ; 現在のline
		   (tokens '())); 返すやつ(逆向き)
	  
	  (define (flash-chars chars tokens line)
	    (if (null? chars)
		tokens
		(cons (cons (list->string (reverse chars)) line) tokens)))

	  (if (= i code-len) ;終了だったら、
	      (begin
                (if (> comment-depth 0);コメントが閉じているか
                    (raise-mylang-error! "parse-error" "unclosed comment" line)
                    (reverse (flash-chars chars tokens line))))
              
	      (let ((chr (string-ref plain-code i)))
                
		(cond
                 ((< 0 comment-depth)
                  (cond
                   ((char=? #\newline chr)
                    (loop (+ i 1) '() #f comment-depth  #f (+ line 1) tokens))
                   ((char=? #\( chr)
                    (loop (+ i 1) '() #f (+ comment-depth 1) #f  line tokens))
                   ((char=? #\) chr)
                    (loop (+ i 1) '() #f (- comment-depth 1) #f line tokens))
                   (else (loop (+ i 1) '() #f comment-depth #f line tokens))))

                 ((and (not in-str?) (not in-line-cmt?) (char=? #\( chr))
                  (loop (+ i 1) '() #f 1 #f line (flash-chars chars tokens line)))

                 ((and (not in-str?) (not in-line-cmt?) (char=? #\) chr))
                  (raise-mylang-error! "parse-error" "extra closing }!" line))


                 (in-line-cmt?
                  (if (char=? #\newline chr)
                      (loop (+ i 1) '() #f 0 #f (+ line 1) tokens)
                      (loop (+ i 1) '() #f 0 #t line tokens)))

                 ((and (not in-str?) (= comment-depth 0) (char=? cmt-chr chr))
                  (loop (+ i 1) '() #f 0 #t line (flash-chars chars tokens line)))

                 
		 ((char=? #\" chr)
		  (loop (+ i 1)
			(cons chr chars)
			(not in-str?)
                        0
                        #f
			line
			tokens))

		 (in-str?
		  (loop (+ i 1)
			(cons chr chars)
			in-str?
                        0
                        #f
			line
			tokens))

		 ((char=? #\newline chr)
		  (loop (+ i 1)
			'()
			#f
                        0
                        #f
			(+ line 1)
			(flash-chars chars tokens line)))

		 ((or (char=? #\space chr) (char=? #\tab chr) (char=? #\, chr));試験的に,も
		  (loop (+ i 1)
			'()
			#f
                        0
                        #f
			line
			(flash-chars chars tokens line)))

		 (else
		  (loop (+ i 1)
			(cons chr chars)
			#f
                        0
                        #f
                        line
			tokens))))))))))

