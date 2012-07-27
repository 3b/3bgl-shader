(in-package #:glsl)

;;; definitions for CL macros supported by the shader DSL
;;;   (and maybe some extra utilities)

(defparameter *glsl-base-environment* (make-instance '3bgl-shaders::cl-environment))

(defclass glsl-walker (3bgl-shaders::cl-walker)
  ())


#++
(let ((a nil))
  (do-external-symbols (s (find-package :cl)
                          (sort a 'string<))
    (when (and (fboundp s) (not (special-operator-p s))
               (macro-function s))
      (push s a))))

;;; stuff that is (or at least could be) handled by compiler:
;;;   all args are (potentially) evaluated normally, etc
;;; AND, DECF, INCF, OR, 

;;; not implementing (either not meaningful/useful or too hard)
;;;   (clos, runtime exceptions, etc)
(loop for s in
      '(CALL-METHOD ASSERT CCASE CTYPECASE DEFCLASS DEFGENERIC
        DEFINE-CONDITION DEFINE-METHOD-COMBINATION DEFMETHOD
        DEFPACKAGE DESTRUCTURING-BIND DO-ALL-SYMBOLS
        DO-EXTERNAL-SYMBOLS DO-SYMBOLS ECASE ETYPECASE FORMATTER
        HANDLER-BIND HANDLER-CASE IGNORE-ERRORS IN-PACKAGE
        MULTIPLE-VALUE-BIND MULTIPLE-VALUE-LIST MULTIPLE-VALUE-SETQ
        NTH-VALUE PPRINT-EXIT-IF-LIST-EXHAUSTED PPRINT-LOGICAL-BLOCK
        PPRINT-POP PRINT-UNREADABLE-OBJECT PROG PROG* PUSHNEW
        RESTART-BIND RESTART-CASE STEP TIME TRACE UNTRACE
        WITH-CONDITION-RESTARTS WITH-HASH-TABLE-ITERATOR
        WITH-COMPILATION-UNIT WITH-INPUT-FROM-STRING WITH-OPEN-FILE
        WITH-OPEN-STREAM WITH-OUTPUT-TO-STRING WITH-PACKAGE-ITERATOR
        WITH-SIMPLE-RESTART WITH-STANDARD-IO-SYNTAX)
      do (3bgl-shaders::add-macro-to-env
          *glsl-base-environment*
          s
          `(lambda (&rest r)
             (declare (ignore r))
             (error ,(format nil "~a not supported in GLSL" s)))))

;;; not sure if we will have some 'list' equivalent?
(loop for s in
      '(POP DOLIST PUSH REMF)
      do (3bgl-shaders::add-macro-to-env
          *glsl-base-environment*
          s
          `(lambda (&rest r)
             (declare (ignore r))
             (error ,(format nil "~a not supported in GLSL" s)))))


;;; maybe?
(loop for s in
      '(DECLAIM CHECK-TYPE DEFTYPE DEFINE-SETF-EXPANDER DEFSETF LAMBDA TYPECASE
        WITH-ACCESSORS WITH-SLOTS)
      do (3bgl-shaders::add-macro-to-env
          *glsl-base-environment*
          s
          `(lambda (&rest r)
             (declare (ignore r))
             (error ,(format nil "~a not supported in GLSL (yet?)" s)))))

;;; todo:
(defmacro %glsl-macro (name lambda-list &body body)
  `(let ((3bgl-shaders::*environment* *glsl-base-environment*))
     (3bgl-shaders::add-macro ',name
                              (lambda (form env)
                                (declare (ignorable env))
                                (destructuring-bind ,lambda-list
                                    (cdr form)
                                  ,@body)))))

(%glsl-macro case (&body body)
  (declare (ignore body))
  `(error "CASE not implemented yet for GLSL."))


(%glsl-macro cond (&body body)
  (if (eq (caar body) t)
      `(progn ,@(cdar body))
      `(if ,(caar body)
           (progn ,@(cdar body))
           ,@(when (cdr body)
               `((cond ,@(cdr body)))))))

(%glsl-macro define-compiler-macro (&body body)
  (declare (ignore body))
  `(error "DEFINE-COMPILER-MACRO not implemented yet for GLSL"))

(%glsl-macro define-modify-macro (&body body)
  (declare (ignore body))
  `(error "DEFINE-MODIFY-MACRO not implemented yet for GLSL"))

(%glsl-macro define-symbol-macro (name expansion)
  (3bgl-shaders::add-symbol-macro name expansion)
  nil)

(%glsl-macro defmacro (name lambda-list &body body)
  ;; fixme: extract docstrings/declarations from body
  (3bgl-shaders::add-macro name
                           `(lambda (form env)
                              (destructuring-bind ,lambda-list
                                  (cdr form)
                                ,@body)))
  nil)

(%glsl-macro defstruct (&body body)
  (declare (ignore body))
  `(error "DEFSTRUCT not implemented yet for GLSL"))

;;; no 'unbound' variables in GLSL, so requiring value arg, and
;;; not sure there is any distinction between DEFVAR and DEFPARAMETER
;;; in shaders, so just expanding to defparameter...
(%glsl-macro defvar (name value &optional docs)
  (declare (ignore docs))
  `(defparameter ,name ,value))

(%glsl-macro do (&body body)
  (declare (ignore body))
  `(error "DO not implemented yet for GLSL"))

(%glsl-macro do* (&body body)
  (declare (ignore body))
  `(error "DO* not implemented yet for GLSL"))

(%glsl-macro dotimes (&body body)
  (declare (ignore body))
  `(error "DOTIMES not implemented yet for GLSL"))

(%glsl-macro loop (&body body)
  (declare (ignore body))
  `(error "LOOP not implemented yet for GLSL"))

(%glsl-macro loop-finish (&body body)
  (declare (ignore body))
  `(error "LOOP-FINISH not implemented yet for GLSL"))

(%glsl-macro prog1 (first-form &body form*)
  (alexandria:with-gensyms (temp)
    `(let ((,temp ,first-form))
       ,@(loop for (a . b) on form*
               while b
               collect `(setf ,a ,(car b)))
       ,temp)))

(%glsl-macro prog2 (first-form second-form &rest form*)
  (alexandria:with-gensyms (temp)
    `(progn
       ,first-form
       (let ((,temp ,second-form))
         ,@(loop for (a . b) on form*
                while b
                collect `(setf ,a ,(car b)))
         ,temp))))

(%glsl-macro PSETF (&body body)
  `(,@body))

(%glsl-macro PSETQ (&body body)
  `(,@body))

;; handle by compiler for now?
#++
(%glsl-macro return (&body body)
 `(,@body))

(%glsl-macro rotatef (&body args)
  (when (cdr args) ;; ignore it if 1 or fewer places
      (alexandria:with-gensyms (temp)
        `(let ((,temp ,(car args)))
           ,@(loop for (a . b) on args
                   while b
                   collect `(setf ,a ,(car b)))
           (setf ,(car (last args)) ,temp)
           ;; rotatef returns NIL
           nil))))

(%glsl-macro setf (&body pairs)
  ;; for now, just expand to a bunch of SETQ forms and hope the
  ;; compiler can deal with them
  ;; (probably implementing things like swizzles and maybe struct
  ;;  accesse at that level, so should be enough for a while, but
  ;;  will probably eventually want to be able to do stuff like
  ;;  (setf (ldb...) foo) etc.)
  (if (> (length pairs) 2)
      `(progn ,@(loop for (a b) on pairs by #'cddr
                      collect `(setq ,a ,b)))
      `(setq ,(first pairs) ,(second pairs))))

(%glsl-macro shiftf (&rest args)
  (alexandria:with-gensyms (temp)
    `(let ((,temp ,(car args)))
       ,@(loop for (a . b) on args
               while b
               collect `(setf ,a ,(car b)))
       ,temp)))

(%glsl-macro unless (a b)
  ;; not quite usual expansion, since we don't really have a "NIL" to return
  `(if (not ,a) ,b))

(%glsl-macro when (a b)
  `(if ,a ,b))



;;; extra 'special forms' for GLSL stuff
;;;   probably expanded directly by compiler

(3bgl-shaders::defwalker glsl-walker (defparameter name value &optional docs)
  (declare (ignore docs))
  `(:var ,name :init ,(3bgl-shaders::@ value)))

(3bgl-shaders::defwalker glsl-walker (defconstant name value  &optional docs)
  (declare (ignore docs))
  `(:var ,name :init ,(3bgl-shaders::@ value) :qualifiers ,(list :const)))

(3bgl-shaders::defwalker glsl-walker (defun name lambda-list &body body+d)
  (multiple-value-bind (body declare doc) (alexandria:parse-body body+d)
    (`(:function ,name :lambda-list ,lambda-list
       :declare ,@(when declare (list declare))
       :doc ,@(when doc (list doc))
       :body ,@(3bgl-shaders::@@ body)))))

(3bgl-shaders::defwalker glsl-walker (:function name &rest args &key body &allow-other-keys)
  (let ((walked (3bgl-shaders::@ body)))
    (when (not (equal walked body))
      (setf args (copy-list args))
      (setf (getf args :body) walked)))
  `(:var ,name ,@args))

(3bgl-shaders::defwalker glsl-walker (:var name &rest args &key init &allow-other-keys)
  (let ((walked (3bgl-shaders::@ init)))
    (when (not (equal walked init))
      (setf args (copy-list args))
      (setf (getf args :init) walked)))
  `(:var ,name ,@args))



#++
(let ((3bgl-shaders::*environment*
        (make-instance '3bgl-shaders::cl-environment
                       :parent *glsl-base-environment*)))
  (3bgl-shaders::walk '(progn
                        (defmacro do-stuff (a)
                          `(doing-stuff ,a))
                        (cond
                          ((= foo 1)
                           (do-stuff foo)
                           (more foo = 1))
                          ((= bar 2))
                          (t (default stuff))))
                      (make-instance '3bgl-shaders::cl-walker)))