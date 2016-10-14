;;; Copyright (C) 2011-2013 Alessio Stalla
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :bacteria)

(defstruct cell
  ;;Ugly kludge: these are explicitly initialized to (list) to make parenscript
  ;;generate [] rather than null.
  (value-generator (list)) (computed-value (list)) (direct-dependents (list))
  (direct-dependencies (list)) (observers (list))
  (event t) (activation-function (lambda (cell event) (declare (ignore cell)) event)))

;;TODO cell garbage collection requires unlinking dependencies manually: provide
;;an API?
;;TODO investigate multithreading issues; key idea: the application manages
;;multithreading, bacteria not thread-safe by design; BUT need to carefully
;;document how each operation is affected by concurrency.

(defvar *cell*)
(defvar *dependencies*)

(defmacro cell (expr &rest args)
  (if (and (listp expr) (eq (car expr) 'cell-value) (null (cddr expr)))
      (cadr expr) ;;Optimize (cell (cell-value foo)) to foo
      `(make-cell :value-generator (lambda (self) (declare (ignorable self)) ,expr) ,@args)))

(defun refresh-cell (cell)
  (let ((*cell* cell))
    ;;reset dependencies
    (dolist (c (cell-direct-dependencies cell))
      (setf (cell-direct-dependents c)
            (remove cell (cell-direct-dependents c))))
    (setf (cell-direct-dependencies cell) (list)) ;;See defstruct cell
    (let ((old-value (cell-computed-value cell))
          (computed-value (funcall (cell-value-generator cell) cell)))
      (setf (cell-computed-value cell) computed-value)
      ;;      (format t "refreshing cell ~A~%" (cell-computed-value cell))
      (notify-observers cell old-value)
      ;;Observers must have a chance to see the event
      (setf (cell-event cell) #-bacteria.js nil #+bacteria.js false)
      computed-value)))

(defun record-dependency (dependent dependee)
  (pushnew dependent (cell-direct-dependents dependee))
  (pushnew dependee (cell-direct-dependencies dependent))
  t)

(defun cell-value (cell)
  (when (boundp '*cell*)
    (if (eql *cell* cell)
        (return-from cell-value (cell-computed-value cell)) ;recursive dep - (setf a (1+ a))
        (record-dependency *cell* cell)))
  (if (cell-event cell)
      (refresh-cell cell)
      (cell-computed-value cell)))

(defun null? (obj)
  #-bacteria.js (not obj) #+bacteria.js (equal obj (list)))

(defun propagate-event (cell &optional (event t))
  (let ((event (funcall (cell-activation-function cell) cell event)))
    (when event
      (setf (cell-event cell) event)
      (when (and (not (null? (cell-observers cell))) (cell-event cell))
        (refresh-cell cell))
      (loop :for dep :in (cell-direct-dependents cell) :do (propagate-event dep event)))
    (cell-event cell)))

(defun notify-observers (cell old-value)
  (let ((obs (cell-observers cell)))
    (when (not (null? obs))
      (let ((new-value (cell-computed-value cell)))
        (unless (eql old-value new-value) ;;TODO allow to customize test fn?
          (dolist (o (cell-observers cell))
            ;;TODO really ignore-errors?
            (ignore-errors (funcall o cell old-value new-value)))))))
  (values))

#+bacteria.js
(defsetf cell-value (cell &optional key event) (value) ;;args to work around PS bug
  `(set-cell-value ,cell ,value ,@(if (keywordp key) (list key event) nil)))

#-bacteria.js
(defsetf cell-value (cell &key (event t)) (value) ;;args to work around PS bug
  `(set-cell-value ,cell ,value :event ,event))

(defmacro set-cell-value (cell value &key (event t))
  ;;todo check if value really changed
  (let ((cval (gensym "CELL")) (evt (gensym "EVENT")) (val (gensym "VALUE")))
    `(let ((,cval ,cell) (,evt ,event) (,val ,value))
       (setf (cell-value-generator ,cval)
             (lambda (,cval)
               (declare (ignore ,cval))
               ,val))
       (propagate-event ,cval ,evt)
       ,val)))

(defun add-observer (cell observer &key (fire-immediately? t))
  (push observer (cell-observers cell))
  (when fire-immediately?
    (if (cell-event cell)
        (refresh-cell cell) ;;Force recomputation and notification
        (funcall observer cell (cell-computed-value cell) (cell-computed-value cell))))
  observer)

(defun remove-observer (cell observer)
  (setf (cell-observers cell) (remove observer (cell-observers cell))))

(defmacro with-cells ((&rest bindings) &body body &environment env)
  (let ((symbols (mapcar #'(lambda (x) (gensym (symbol-name (car x)))) bindings))
        (the-old-cell (gensym (symbol-name 'the-old-cell))))
    `(let ,symbols
       (symbol-macrolet ,(mapcar #'(lambda (sym binding) `(,(car binding) (cell-value ,sym)))
                                 symbols bindings)
         (macrolet ((,the-old-cell (name)
                      (macroexpand `(the-cell ,name) ,env))
                    (the-cell (name)
                      (let ((pos (position name ',bindings :key #'car)))
                        (if pos
                            (nth pos ',symbols)
                            `(,',the-old-cell ,name)))))
           (setf ,@(mapcan #'(lambda (sym binding)
                               (let ((init-val (cadr binding))
                                     (keys (cddr binding)))
                                 (remf keys :observer)
                                 `(,sym (cell ,init-val ,@keys))))
                           symbols bindings))
           ,@(remove nil
                     (mapcar #'(lambda (sym binding)
                                 (when (getf (cddr binding) :observer) ;;TODO fire-immediately?
                                   `(add-observer ,sym ,(getf (cddr binding) :observer))))
                             symbols bindings))
           ,@body)))))

(defmacro defcell (name &optional initial-value)
  (let ((var (gensym (symbol-name name))))
    `(progn
       (defvar ,var (cell ,initial-value))
       (define-symbol-macro ,name (cell-value ,var))
         #-bacteria.js (setf (get ',name 'the-cell) ,var))))

(defmacro the-cell (name)
  (alexandria:once-only (name)
    `(or #-bacteria.js (get ',name 'the-cell)
         #+bacteria.js (error "Top-level the-cell not supported")
         (error "No cell named ~A exists" ',name))))

(defmacro cell? (name)
  (alexandria:once-only (name)
    `(or (ignore-errors (the-cell ,name)) (cell-p ,name))))

#+parenscript
#-bacteria.js
(progn
  (defparameter *this-file* #.(or *compile-file-pathname* *load-truename*))

  (defun compile-to-js ()
    (let ((*features* (cons :bacteria.js *features*)))
      (when (not (find-package :defstruct.js))
        ;;TODO use ASDF?
        (load (compile-file (merge-pathnames
                             (make-pathname :name "defstruct.js" :type "lisp")
                             *this-file*))))
      (concatenate 'string
                   (ps:ps* (funcall (find-symbol "GENERATE-DEFSTRUCT-JS-RUNTIME"
                                                 (find-package :defstruct.js))))
                   (ps:ps-compile-file (merge-pathnames
                                        (make-pathname :name "bacteria.js" :type "lisp")
                                        *this-file*))
                   (ps:ps-compile-file *this-file*)))))

#||
(setf x (cell 42))
(setf y (cell (* 10 (cell-value x))))
(setf z (cell (/ (cell-value y) 2)))
(setf k (cell (* (cell-value y) 2)))
(setf b (cell t))
(setf m (cell (if (cell-value b) (cell-value z) (cell-value k))))

(cell-value y)
(cell-value z)

(setf (cell-value x) 12)
(cell-value k)
(cell-value z)

(add-observer z (lambda (val) (print 'z1) (print val)))
(add-observer z (lambda (val) (print 'z2) (print val)))
(add-observer m (lambda (val) (print 'm) (print val)))
(add-observer x (lambda (val) (print 'x) (print val)))
(setf (cell-value x) 44)
||#