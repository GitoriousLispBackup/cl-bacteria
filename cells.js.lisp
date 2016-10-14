;;Modifications to bacteria to make it more js-friendly. To be loaded AFTER
;;bacteria. This is an optional module; you don't need to load it if you only
;;use bacteria from CL using Parenscript.

;;Cell constructor
(defun mkcell (value)
  (let ((c (make-cell :value-generator (if (ps:instanceof value *function)
                                           value
                                           (lambda () value)))))
    (ps:for-in (x c)
      (setf (ps:getprop this x) (ps:getprop c x)))))

(defmacro define-method (name (obj &rest args) &body body)
  `(setf (ps:@ ,obj prototype ,name)
      (lambda ,args ,@body)))

(define-method get-value (cell)
  (cell-value this))

(define-method set-value (cell value)
  (setf (cell-value this) value))

(define-method add-observer (cell obs)
  (add-observer this obs))

(define-method remove-observer (cell obs)
  (remove-observer this obs))
