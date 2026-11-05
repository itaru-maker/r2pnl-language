(defstruct true-value)
(defvar *the-true* (make-true-value))

(defstruct false-value)
(defvar *the-false* (make-false-value))

(defstruct nil-value)
(defvar *the-nil* (make-nil-value))

(defstruct symbol-value token)
(defstruct quote-value token)
(defstruct block-value items)
(defstruct builtin-func name body)
(defstruct proc-value body line file name)
(defstruct error-value name message)

;;token系
(defstruct rparen)
(defvar *the-rparen* (make-rparen))

(defstruct lparen)
(defvar *the-lparen* (make-lparen))

(defstruct list-marker)
(defvar *the-list-marker* (make-list-marker))

(defun value->write-string (v)
  (typecase v
    (number (format nil "~a" v))
    (string (format nil "\"~a\"" v))
    (true-value "#true")
    (false-value "#false")
    (nil-value "#nil")
    (symbol-value (symbol-value-token v))
    (quote-value (format nil "'~a" (quote-value-token v)))
    (block-value
     (concatenate "{ "
      (apply #'string-append (map '#(lambda (pair) (concatenate (value->write-string (car pair)) " "))))
      "}"))
    (builtin-func
     (format nil "<builtin: ~a >" (builtin-func-name v)))
    (proc-value
     (format nil "<proc: ~a >" (proc-value-name)))
    (rparen "<rparen>")
    (lparen "<lparen>")
    (list-marker "<list-marker>")
    ;;(error-value (format nil "<error ~a : ~a >" (error-value-name v) (error-value-message v)))
    (t (error (format nil "debug:unknowntype:~a" v)))))

(defun value->display-string (v)
  (typecase v
    (string v)
    (t (value->write-string v))))
