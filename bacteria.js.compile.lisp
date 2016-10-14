(in-package :bacteria)

(defun compile-bacteria-to-js ()
  (let ((*features* (cons :bacteria.js *features*))
        (base-path (load-time-value *load-truename*)))
    (ps:ps-compile-file
     (merge-pathnames (make-pathname :name "bacteria.js" :type "lisp") base-path))
    (ps:ps-compile-file
     (merge-pathnames (make-pathname :name "bacteria" :type "lisp") base-path))))