(defpackage :defstruct.js
  (:use :cl :parenscript)
  (:export #:generate-defstruct-js-runtime #:defstruct/json))

(in-package :defstruct.js)

(defun keywordize (symbol)
  (intern (symbol-name symbol) :keyword))

(defun slot-spec-name (slot-spec)
  (if (listp slot-spec) (car slot-spec) slot-spec))

(defun slot-spec-initform (slot-spec)
  (if (listp slot-spec) (cadr slot-spec) nil))

(defun ensure-list (obj)
  (if (listp obj) obj (list obj)))

(defun make-constructor-name (name)
  (intern (concatenate 'string "MAKE-" (symbol-name name))))

(defun generate-struct-js-constructor (name slots &optional included)
  "Generates a <name> constructor function that can be used to instantiate a <name> structure with new <name>(...), that is, (ps:new (<name> ...)). If `included` is supplied, it will be invoked first to initialize the included structure."
  `(defun ,name (&key ,@slots) ;;TODO slot options? check, allow?
     ,(when included
       `(funcall ,included this))
     (setf (ps:getprop this :__type) ',name)
     ,@(loop
          :for slot :in slots
          :collect `(setf (ps:getprop
                           this
                           ,(ps:symbol-to-js-string (slot-spec-name slot)))
                          ,(slot-spec-name slot)))
     this))

(defun generate-defstruct-js-runtime ()
  `(progn
     (defvar *type-system* (ps:new *Object))
     (defmacro deftype (name &optional (supertypes '(list *Object)))
       `(setf (ps:getprop *type-system* ',name)
              (ps:create :name ',name
                         :to-string (lambda () (ps:getprop this :name))
                         :supertypes ,supertypes
                         :__type 'type)))
     (deftype type)
     (defun find-type (name)
       (ps:getprop *type-system* name))
     (defun type-of (obj)
       (if (not (eql (ps:getprop obj '__type) undefined))
           (find-type (ps:getprop obj '__type))
           (ps:typeof obj))) ;;TODO include those in the type system
     (defun canonicalize-type (type)
       (if (= (+ (type-of type) "") 'type)
           type
           (find-type type)))
     (defun subtypep (type1 type2)
       (let ((t1 (canonicalize-type type1))
             (t2 (canonicalize-type type2)))
         (if (= t1 t2)
             t
             (loop
                :for type :in (ps:getprop t1 :supertypes)
                :when (subtypep type t2)
                :do (return t)))
         ps:f))))

(defmacro defstruct-bind ((name-var &rest keys) arglist &body body)
  (let ((name-and-options (gensym)) (option (gensym)))
    `(let ((,name-and-options (ensure-list ,arglist)))
       (let ((,name-var (car ,name-and-options))
             ,@keys)
         (dolist (,option (cdr ,name-and-options))
           (cond
             ,@(loop
                  :for key :in (mapcar (lambda (k)
                                         (etypecase k
                                           (list (car k))
                                           (symbol k)))
                                       keys)
                  :collect `((string= ,(string key) (string (car ,option)))
                             (setf ,key (cdr ,option))))))
         ,@body))))

(ps:defpsmacro defstruct/json (name-and-options &rest slots)
  (defstruct-bind (name (constructor-style :js+cl) include)
      name-and-options
    (flet ((slot-accessor-name (slot)
             (intern (concatenate 'string
                                  (symbol-name name) "-"
                                  (symbol-name (slot-spec-name slot))))))
      `(progn
         ,@(ecase constructor-style
             (:js (list (generate-struct-js-constructor name slots include)))
             (:js+cl (list
                      (generate-struct-js-constructor name slots include)
                      `(defun ,(make-constructor-name name) ()
                         (funcall (ps:getprop ,name 'apply) (ps:create) arguments)))))
         ;;TODO when :include, add included slots
         ,@(flet ((make-reader-definition (slot)
                   `(defun ,(slot-accessor-name slot) (obj)
                       (ps:getprop obj ',(slot-spec-name slot)))))
                 (mapcar #'make-reader-definition slots))
         ,@(flet ((make-writer-definition (slot)
                    `(defsetf ,(slot-accessor-name slot) (obj) (val)
                       `(setf (ps:getprop ,obj ',',(slot-spec-name slot)) ,val))))
                 (mapcar #'make-writer-definition slots))
         nil))))

(ps:defpsmacro defstruct (name-and-options &rest slots)
  "Turns an ordinary Lisp defstruct, compiled by Parenscript, into a JSON struct definition."
  `(defstruct/json ,name-and-options ,@slots))

#+cl-json
(defvar *defstruct/json-recursive-p*)

#+cl-json
(defmacro defstruct/json (name-and-options &rest slots)
  "Allows to define JSON-aware structures in Lisp, using CL-JSON. This will additionally define a method on json:encode-json that will encode instances of the newly defined structure in JSON objects as would be created in JS by defstruct.js."
  `(prog1
       (defstruct ,name-and-options ,@slots)
     ,(defstruct-bind (name constructor-style include)
          name-and-options
        ;(declare (ignore constructor-style))
        (let* ((obj (gensym)) (stream (gensym))); (recursivep (gensym))
               (encode-code
                `(progn
                   ;;TODO handle duplicate slots
                   ,(when include `(let (*defstruct/json-recursive-p*)
                                     (call-next-method ,obj ,stream)))
                   ,@(loop
                        :for slot :in slots
                        :collect `(json:as-object-member
                                   ((quote ,(slot-spec-name slot))
                                    ,stream)
                                   (json:encode-json ,(slot-spec-name slot) ,stream))))))
          `(defmethod json:encode-json ((,obj ,name) &optional
                                        (,stream json:*json-output*))
             (let ((ps:*parenscript-stream* ,stream)
                   ,@(mapcar (lambda (slot-spec)
                               (let ((slot-name (slot-spec-name slot-spec)))
                                 `(,slot-name
                                   (,(intern (concatenate 'string (symbol-name name) "-" (symbol-name slot-name)))
                                     ,obj))))
                             slots))
               (print (boundp '*defstruct/json-recursive-p*))
               (if (boundp '*defstruct/json-recursive-p*)
                   ,encode-code
                   (json:with-object (,stream)
                     ,encode-code)))))))