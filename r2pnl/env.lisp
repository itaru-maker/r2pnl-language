(defstruct env vars)

(defun make-env ()
  (make-env '()))

(defun in-env? (env name)
  (if (assoc name (env-vars env)) t nil :test #'equal))

(defun env-lookup (env name)
  (assoc name (env-vars env) :test #'equal?))

(defun env-define (env name value)
  (setf (env-vars env)
        (cons (cons name value) (env-vars env))))

(defun env-set! (env name value)
  (let ((pair (assoc name (env-vars env))))
    (if pair
        (setf (cdr pair) value))))
