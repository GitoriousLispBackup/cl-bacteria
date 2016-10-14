;;Some CL support functions for executing Bacteria translated to JS. To be loaded BEFORE bacteria.

(defun remove (obj arr)
  (when (or (null arr) (ps:= arr undefined))
    (return arr))
  (dotimes (i (ps:getprop arr 'length))
    (when (= (aref arr i) obj)
      (ps:chain arr (splice i 1))))
  arr)

(defmacro push (value place)
  `(setf ,place (ps:chain (list ,value) (concat ,place))))

(defmacro pushnew (value place)
  `(setf ,place (ps:chain (list ,value) (concat (remove ,value ,place)))))

;;only handles the simple case (boundp 'foo)
(defmacro boundp (quoted-symbol)
  `(not (eql ,(cadr quoted-symbol) undefined)))

(defun constantly (value)
  (lambda () value))