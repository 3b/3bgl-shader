(in-package #:3bgl-shaders)

(defclass walker ()
  ())

#++
(defmacro define-walker ()
  )

(defgeneric walk (form walker))
(defgeneric walk-cons (car cdr walker))

(defmethod walk (form walker)
  form)

(defmethod walk ((form cons) walker)
  (walk-cons (car form) (cdr form) walker))

(defmethod walk-cons (car cdr walker)
  (list* car (mapcar (lambda (f) (walk f walker)) cdr)))

(defparameter *environment* nil "current local environment")
(defparameter *global-environment* nil "current global environment")

;; todo: store declarations somewhere in env, and parse them when walking code?
(defclass environment ()
  ((parent-scope :reader parent-scope :initarg :parent :initform nil)
   ;; map of name -> FUNCTION-BINDING instance
   (function-bindings :reader function-bindings :initform (make-hash-table))
   ;; name -> MACRO-DEFINITION instance
   (compiler-macro-bindings :reader compiler-macro-bindings :initform (make-hash-table))
   ;; map of name -> BINDING instance
   (variable-bindings :reader variable-bindings :initform (make-hash-table))
   (types :reader types :initform (make-hash-table))))

(defparameter *cl-environment* (make-instance 'environment))

(defun get-variable-binding (name &key (env *environment*))
  (and env
       (or (gethash name (variable-bindings env))
           (get-variable-binding name :env (parent-scope env)))))

(defun get-function-binding (name &key (env *environment*))
  ;; not sure if this should accept a function-binding object as a
  ;;   designator for itself?
  ;; or if accepting a function-binding, should it instead look up
  ;;   the current binding for that name?
  (and env
       (or (gethash name (function-bindings env))
           (get-function-binding name :env (parent-scope env)))))

(defun get-compiler-macro-binding (name &key (env *environment*))
  ;; find a compiler macro in any env no deeper than current
  ;; function-binding
  ;; (function bindings shadow compiler macro bindings)
  (and env
       (or (gethash name (compiler-macro-bindings env))
           (and (not (gethash name (function-bindings env)))
                (get-compiler-macro-binding name :env (parent-scope env))))))

(defun get-type-binding (name &key (env *environment*))
  (if (consp name)
      (make-instance 'array-type :base-type (get-type-binding (car name)
                                                             :env env)
                     :array-size (second name))
      (and env
           (or (gethash name (types env))
               (get-type-binding name :env (parent-scope env))))))


(defun add-macro (name lambda &key (env *environment*))
  #++
  (assert (not (gethash name (function-bindings env))))
  (setf (gethash name (function-bindings env))
        (make-instance 'macro-definition
                       :name name
                       :expression lambda)))

(defun add-compiler-macro (name lambda &key (env *environment*))
  (setf (gethash name (compiler-macro-bindings env))
        (make-instance 'macro-definition
                       :name name
                       :expression lambda)))

(defun get-compiler-macro-function (name)
  (let ((mf (get-compiler-macro-binding name :env *environment*)))
    (when (typep mf 'macro-definition)
      (when (not (expander mf))
        (setf (expander mf) (compile 'nil (expression mf))))
      (expander mf))))

(defun get-macro-function (name)
  (let ((mf (get-function-binding name :env *environment*)))
    (when (typep mf 'macro-definition)
      (when (not (expander mf))
        (setf (expander mf) (compile 'nil (expression mf))))
      (expander mf))))

(defun get-symbol-macro (name)
  (let ((b (get-variable-binding name :env *environment*)))
    (when (typep b 'symbol-macro)
      b)))

(defun add-symbol-macro (name expansion &key (env *environment*))
  (assert (not (gethash name (variable-bindings env))))
  (setf (gethash name (variable-bindings env))
        (make-instance 'symbol-macro
                       :name name
                       :value-type t
                       :expansion expansion)))

(defun add-variable (name init &key (env *environment*) binding
                                 (type 'variable-binding) value-type)
  ;(assert (not (gethash name (variable-bindings env))))
  (setf (gethash name (variable-bindings env))
        (or binding
            (make-instance type
                           :name name
                           :value-type (or (get-type-binding value-type) T)
                           :init init))))


(defun make-&key-expander (lambda-list)
  (multiple-value-bind (req opt rest key aux)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare (ignorable req))
    (when rest
      (error "&rest not supported"))
    (when aux
      (error "&aux not supported yet"))
    ;; if we have any supplied-p args, we just expand into all required args
    ;; else if we have only optional args, we let glsl default values handle it
    ;; else if we have &key, we expand to optional positional args
    (let ((use-optional (not (or (some 'third opt) (some 'third key))))
          (args (reverse req))
          (bindings (mapcar (lambda (n)
                              (make-instance 'function-argument
                                      :name n
                                      :value-type t
                                      :init nil))
                            (reverse req))))
      (loop for (n i s) in opt
            when n
              do (push n args)
                 (push (make-instance 'function-argument
                                      :name n
                                      :value-type t
                                      :init (when use-optional i))
                       bindings)
            when s
              do (push `(if ,s 1 0) args)
                 (push (make-instance 'function-argument
                                      :name s
                                      :value-type (get-type-binding :bool)
                                      :init nil)
                       bindings))

      (loop for ((nil n) i s) in key
            when n
              do (push n args)
                 (push (make-instance 'function-argument
                                      :name n
                                      :value-type t
                                      :init (when use-optional i))
                       bindings)
            when s
              do (push `(if ,s 1 0) args)
                 (push (make-instance 'function-argument
                                      :name s
                                      :value-type (get-type-binding :bool)
                                      :init nil)
                       bindings))
      (values
       (reverse bindings)
       (if (or key (not use-optional))
           (compile nil
                    `(lambda (args)
                       ;; fixme: probably should switch unspecified
                       ;; default values from NIL to 0 or something?
                       (destructuring-bind (,@lambda-list) args
                         (list ,@(reverse args)))))
           #'identity)))))
#++
(make-&key-expander '(a &optional (b 2 p) &key (c 2.0 cp) (d 1)))
#++
(funcall (nth-value 1 (make-&key-expander '(a &optional (b 2 p) &key (c 2.0 cp) (d 1))))
         (list 'a 'b :d 'cc))

(defun add-function (name lambda-list body
                     &key declarations docs (env *global-environment*)
                       (function-type 'global-function)
                       binding)
  (format t "add function ~s~%" name)
  ;;(assert (not (gethash name (function-bindings env))))
  (if binding
      (setf (gethash name (function-bindings env)) binding)
      (multiple-value-bind (bindings expander)
          (make-&key-expander lambda-list)
        (let ((old (gethash name (function-bindings env))))
          ;; reuse instances so we can link to them directly
          ;; from other functions
          (format t "old = ~s (in ~s)~%" old env)
          (flet ((add-or-update (&rest args)
                   (etypecase old
                     (unknown-function-binding
                      (format t "update unknown function ~s~%" (name old))
                      (apply #'change-class old function-type
                             args))
                     (function-binding-function
                      (apply #'reinitialize-instance old
                             args))
                     (null
                      (setf (gethash name (function-bindings env))
                            (apply #'make-instance function-type
                                   args))))))
            (add-or-update :name name
                           ;todo :function-type t
                           :lambda-list lambda-list
                           :bindings bindings
                           :expander expander
                           :body body
                           :docs docs
                           :declarations declarations
                           :value-type t))))))

(defun add-function-arguments (function &key (env *environment*))
  ;; add the bindings from a functions's arglist to current environment
  (loop with v = (variable-bindings env)
        for b in (bindings function)
        for n = (name b)
        do (assert (not (gethash n v)))
           (setf (gethash n v) b)))

(defun add-unknown-function (name &key (env *global-environment*))
  (format t "add unknown function to ~s (in ~s)~%" name env)
  (if (get-function-binding name :env env)
      (get-function-binding name :env env)
      (setf (gethash name (function-bindings env))
            (make-instance 'unknown-function-binding
                           :name name
                           :docs nil
                           :declarations nil
                           :environment env))))


(defclass cl-walker (walker)
  ())

(defmacro with-environment-scope (() &body body)
  `(let ((*environment* (make-instance 'environment :parent *environment*)))
     ,@body))



(defmacro defwalker (walker (form &rest args) &body body)
  (alexandria:with-gensyms (car cdr)
    `(defmethod walk-cons ((,car (eql ',form)) ,cdr (walker ,walker))
       (declare (ignorable ,car))
       (labels ((@ (form) (walk form walker))
                (@@ (forms &key declare)
                    (if (and declare (typep (car forms)
                                            '(cons (member declare))))
                        (cons (car forms) (mapcar #'@ (cdr forms)))
                        (mapcar #'@ forms))))
         (declare (ignorable #'@ #'@@))
         (destructuring-bind ,args ,cdr
           ,@body)))))



;; +block     +let*                 +return-from
;; +catch     +load-time-value      +setq
;; +eval-when +locally              +symbol-macrolet
;; +flet      +macrolet             +tagbody
;; +function  +multiple-value-call  .the
;; +go        +multiple-value-prog1 +throw
;; .if        .progn                unwind-protect
;; +labels     progv
;; +let        quote
;;

;; same as default behavior:
;;  if, progn, the

(defwalker cl-walker (block name &rest body)
  ;; probably should add blocks to environment?
  `(block ,name ,@(@@ body)))

(defwalker cl-walker (return-from name &optional (result nil resultp))
  `(return-from ,name ,@(when resultp (list (@ result)))))

(defwalker cl-walker (catch tag &rest forms)
  `(catch ,(@ tag) ,@(@@ forms)))

(defwalker cl-walker (throw tag result-form)
  `(throw ,(@ tag) ,(@ result-form)))

(defwalker cl-walker (load-time-value form &optional read-only-p)
  `(load-time-value ,(@ form) ,read-only-p))

(defwalker cl-walker (setq &rest assignments)
  `(setq ,@(loop for (a b) on assignments by #'cddr
                 when (nth-value 1 (get-symbol-macro a))
                   do (error "can't expand assignment to symbol macros in SETQ yet (in form ~s)" `(setq ,@assignments))
                 ;; fixme: implement symbol-macro stuff somewhere
                 collect a
                 collect (@ b))))

(defwalker cl-walker (eval-when (&rest situations) &body body)
  `(eval-when ,situations
     ,@(@@ body)))

(defwalker cl-walker (locally declarations &body body)
  `(locally ,declarations ,@(@@ body)))


(defwalker cl-walker (symbol-macrolet (&rest bindings) &rest body)
  ;; not sure if there is any reason to preserve the symbol-macrolet, so
  ;; just letting it expand for now...
  (with-environment-scope ()
    (loop for (name expansion) in bindings
          do (add-symbol-macro name expansion))
    (let ((w (@@ body :declare t)))
      (typecase w
        ((cons (member progn)) w)
        ((cons T NULL) (car w))
        (t (cons 'progn w))))))

(defwalker cl-walker (macrolet (&rest bindings) &rest body)
  ;; not sure if there is any reason to preserve the macrolet, so
  ;; just letting it expand for now...
  (with-environment-scope ()
    (loop for (name lambda-list . body) in bindings
          do (add-macro name
                        `(lambda (form env)
                           (declare (ignorable env))
                           (destructuring-bind ,lambda-list
                               (cdr form)
                             ,@body))))
    (let ((w (@@ body :declare t)))
      (typecase w
        ((cons (member progn)) w)
        ((cons T NULL) (car w))
        (t (cons 'progn w))))))

(defwalker cl-walker (tagbody &body body)
  ;; todo: probably should store go tags in environment
  `(tagbody
      ,@(loop for f in body
              when (consp f)
                collect (@ f)
              else collect f)))

(defwalker cl-walker (go tag)
  `(go ,tag))


(defmacro with-lambda-list-vars ((function) &body body)
  `(with-environment-scope ()
     (mapcar (lambda (a) (add-variable (name a) nil :binding a))
             (bindings ,function))
     ,@body))

(defun walk-function-body (walker lambda-list body)
  ;; fixme: not sure if this gets the scopes quite right when same
  ;; name appears multiple times
  ;; not sure it matters though, since anything that cares probably
  ;; has its own code walker?
  (let* ((declare (when (typep (car body) '(cons (member declare)))
                    (pop body)))
         (walked
           (with-environment-scope ()
             (mapcar #'(lambda (a) (add-variable a nil))
                     (lambda-list-vars lambda-list))
             (mapcar (lambda (a) (walk a walker)) body))))
    (if declare
        (cons declare walked)
        walked)))

(defwalker cl-walker (function name)
  (if (typep name '(cons (member lambda)))
      `(function
        (lambda (second name)
         (walk-function-body (cadr name) (cddr name))))
      `(function name)))

(defwalker cl-walker (multiple-value-call function-form form*)
  `(multiple-value-call ,(@ function-form) ,@(@@ form*)))

(defwalker cl-walker (multiple-value-prog1 first-form form*)
  `(multiple-value-prog1 ,(@ first-form) ,@(@@ form*)))

(defwalker cl-walker (unwind-protect protected-form &rest cleanup-form*)
  `(unwind-protect ,(@ protected-form)
     ,@(@@ cleanup-form*)))

(defwalker cl-walker (progv symbols values &rest form*)
  ;; fixme: should we add dynamic bindings to environment?
  `(progv
       ,(@ symbols)
       ,(@ values)
     ,@(@@ form*)))

(defwalker cl-walker (quote object)
  ;; fixme: should we add dynamic bindings to environment?
  `(quote ,object))


(defwalker cl-walker (let (&rest bindings) &rest body)
  ;; walk default values if any, and body
  `(let (,@(mapcar (lambda (a)
                     (let ((var (if (consp a) (car a) a))
                          (init (if (consp a) (cadr a) nil)))
                      (list var (@ init))))
                   bindings))
     ,@(with-environment-scope ()
         (mapcar (lambda (a) (add-variable (if (consp a) (car a) a) nil))
                 bindings)
         (@@ body :declare t))))

(defwalker cl-walker (let* (&rest bindings) &rest body)
  ;; walk default values if any, and body
  (with-environment-scope ()
    `(let* (,@(mapcar (lambda (a)
                        (let ((var (if (consp a) (car a) a))
                              (init (if (consp a) (cadr a) nil)))
                          (prog1
                              (list var (@ init))
                            (add-variable a nil))))
                      bindings))
       ,@(@@ body :declare t))))

(defun lambda-list-vars (lambda-list)
  (multiple-value-bind (req opt rest keys aux)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (remove 'nil
            (append req
                    (mapcar 'car opt) (mapcar 'third opt)
                    (list rest)
                    (mapcar 'cadar keys)
                    (mapcar 'third keys)
                    (mapcar 'first aux)))))

(defwalker cl-walker (flet (&rest functions) &rest body)
  ;; walk function bodies (with local functions not in scope yet)
  (error "rewrite this...")
  #++
  (let ((walked (loop for (f ll . body+d) in functions
                      for (body declare doc) = (multiple-value-list
                                                (alexandria:parse-body
                                                 body+d :documentation t))
                      for walked = (with-lambda-list-vars (walker ll)
                                     (@@ body))
                      collect (list f ll walked declare doc))))
    ;; then rebuild expanded flet form
    `(flet (,@(mapcar (lambda (a)
                        (destructuring-bind (f ll wbody declare doc) a
                          `(,f ,ll
                               ,@(when declare `(,declare))
                               ,@(when doc `(,doc))
                               ,@wbody)))
                walked))
      ,@(with-environment-scope (walker)
          ;; add local functions to env
          (mapcar (lambda (a)
                    (destructuring-bind (f ll wbody declare doc) a
                      (add-function :local-function f ll wbody declare doc)))
                  walked)
          ;; and walk main body
          (@@ body :declare t)))))

(defwalker cl-walker (labels (&rest functions) &rest body)
  ;; walk function bodies and main body
  (error "rewrite this...")
  #++
  (with-environment-scope (walker)
    ;; add all function names to env (with empty bodies for now)
    ;; so they are in scope while walking bodies (even if we can't
    ;; have recursive functions, they still should shadow
    ;; any enclosing macro definitions and such)
    (mapcar (lambda (a)
              (add-function :local-function (first a) (second a) nil nil nil))
            functions)
    ;; walk function bodies, and update definitions in env
    `(labels (,@(loop for (f ll . body+d) in functions
                      for (body declare doc) = (multiple-value-list
                                                (alexandria:parse-body
                                                 body+d :documentation t))
                      for walked = (with-lambda-list-vars (walker ll)
                                     (@@ body))
                      ;; fixme: figure out how to do this without env internals
                      for fenv = (gethash f (function-bindings *environment*))
                      do (setf (getf (cdr fenv) :body) walked)
                         (setf (getf (cdr fenv) :declare) declare)
                         (setf (getf (cdr fenv) :doc) doc)
                      collect `(,f ,ll
                                   ,@(when declare (list declare))
                                   ,@(when doc (list doc))
                                   ,@walked)))
       ;; and walk main body
       ,@(@@ body :declare t))))


(defmethod walk-cons (car cdr (walker cl-walker))
  ;; anything that isn't a special form, try expanding as macro, otherwise
  ;; expand normally
  ;; fixme: rearrange this so we can hook function application without
  ;; having to check for macros by hand
  (let* ((macro (get-macro-function car))
         (cmacro (unless macro
                   (get-compiler-macro-function car)))
         (form (list* car cdr)))
    #++(format t "check for macro ~s -> ~s / ~s~%" car macro cmacro)
    (cond
      (cmacro
       (let ((expanded (funcall cmacro form *environment*)))
         (if (eq expanded form)
             (if macro
                 (walk (funcall macro form *environment*) walker)
                 (call-next-method))
             (walk expanded walker))))
      (macro
       (walk (funcall macro form *environment*) walker))
      (t
       (call-next-method)))))

(defmethod walk (form (walker cl-walker))
  ;; expand symbol macros
  (multiple-value-bind (macro mp) (get-symbol-macro form)
    (if mp
        (walk macro walker)
        (call-next-method))))

(defparameter *depth* 1)
(defparameter *max-depth* 100)

(defmethod walk :around (form walker)
  (let ((*depth* (1+ *depth*)))
    (when (> *depth* *max-depth*)
      (error "nesting depth too deep"))
    (call-next-method)))

(defmethod walk-cons :around (car cdr walker)
  (let ((*depth* (1+ *depth*)))
    (when (> *depth* *max-depth*)
      (error "nesting depth too deep"))
    (call-next-method)))


(defmacro defclmacro (name lambda-list &body body)
  `(let ((*environment* *cl-environment*)
         (*global-environment* *cl-environment*)
)
     (add-macro ,name
                (lambda (form env)
                  (declare (ignorable env))
                  (destructuring-bind ,lambda-list
                      (cdr form)
                    ,@body)))))

#++
(walk '(macrolet ((a (b c)
                   `(+ 1 ,b 2 ,c 3)))
        (symbol-macrolet ((a (eq 'a 4))
                          (b (eq 'b 5 (a 6 7))))
          (let ((a 1))
            (a a b))))
      (make-instance 'cl-walker))
