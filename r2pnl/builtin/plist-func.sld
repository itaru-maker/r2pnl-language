(define-library (r2pnl builtin plist-func)
  (export plist-func-dict)
  (import
   (scheme base)
   (scheme write)
   (r2pnl values)
   (r2pnl interpreter)
   (r2pnl builtin comp-func))
  (begin
    (define (plist-has? plist key interp)
      (let ((items (block-value-items plist)))
        (if (odd? (length items))
            (interp-error! interp "ValueError" "the \"plist-has?\" func expects an even length list")
            (let loop ((rest items))
              (cond 
               ((null? rest) #f)
               ((value-equal? key (caar rest)) #t)
               (else (loop (cddr rest))))))))

    (define (plist-lookup plist key interp)
      (let ((items (block-value-items plist)))
        (cond ((odd? (length items))
               (interp-error! interp "ValueError" "the \"plist-has?\" func expects an even length list"))
              ((not (plist-has? plist key interp))
               (interp-error! interp "KeyError" (string-append "the \"plist-lookup\" func cannot found key:" (value->write-string key))))
              (else (let loop ((rest items))
                      (cond 
                       ((value-equal? key (caar rest)) (car (cadr rest)))
                       (else (loop (cddr rest)))))))))
    
    (define (plist-get-func interp)
      (let* ((key (stack-pop! interp))
             (plist (stack-pop! interp)))
        (if (block-value? plist)
            (stack-push! interp (plist-lookup plist key interp))
            (interp-error! interp "TypeError" "the \"plist-lookup\" func expects a plist and an any value"))))
    
    (define (define-plist plist key value interp)
      (let ((items (block-value-items plist))
            (line (interp-token-line interp)))
        (if (not (plist-has? plist key interp))
            (make-block-value
             (cons (cons key (interp-token-line interp))
                   (cons (cons value (interp-token-line interp)) items)))
            (let loop ((acc '()) (rest items))
              (if (value-equal? key (caar rest))
                  (make-block-value
                   (append (reverse acc)
                           (cons (car rest) 
                                 (cons (cons value line)
                                       (cddr rest)))))
                  (loop (cons (cadr rest) (cons (car rest) acc))
                        (cddr rest)))))))

    
    (define (plist-has?-func interp)
      (let* ((key (stack-pop! interp))
             (plist (stack-pop! interp)))
        (if (block-value? plist)
            (stack-push! interp (plist-has? plist key interp))
            (interp-error! interp "TypeError" "the \"plist-has?\" func expects a plist and an any value"))))
    
    (define (define-plist-func interp)
      (let* ((value (stack-pop! interp))
             (key (stack-pop! interp))
             (plist (stack-pop! interp)))
        (if (block-value? plist)
            (stack-push! interp (define-plist plist key value interp))
            (interp-error! interp "TypeError" "the \"define-plist\" expects a block"))))
    

    (define plist-func-dict
      `(("define-plist" . ,define-plist-func)
        ("plist-has?" . ,plist-has?-func)
        ("plist-get" . ,plist-get-func)))))
