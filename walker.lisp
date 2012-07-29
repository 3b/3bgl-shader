(in-package #:3bgl-shaders)

(defclass walker ()
  ())

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

(defparameter *environment* nil)

;; todo: store declarations somewhere in env, and parse them when walking code?
(defclass cl-environment ()
  ((parent-scope :reader parent-scope :initarg :parent :initform nil)
   ;; map of name -> (type . args)
   ;; where TYPE is :macro or :function
   ;;  if :function, args is lambda-list
   ;;  if :macro, args is (lambda compiled-function)
   ;;    where lambda is a LAMBDA form to be compiled to get compiled-function
   ;;    and compiled-function might be NIL if not compiled yet
   (function-bindings :reader function-bindings :initform (make-hash-table))
   ;; map of name -> (type . args)
   ;; where TYPE is :symbol-macro or :variable
   ;;   if :symbol-macro, args is expansion
   (variable-bindings :reader variable-bindings :initform (make-hash-table))))

(defparameter *cl-environment* (make-instance 'cl-environment))

;; these leave existing values, in case we already added some metadata
;; to them (mainly for globals, which might reasonably be processed
;; multiple times in the same scope)
(defmethod add-macro-to-env ((env cl-environment) name lambda)
  ;; todo: complain about existing bindings in this scope?
  ;; (but don't want to complain about redefining globals)
  (setf (gethash name (function-bindings env))
        (list :macro lambda nil)))

(defmethod add-symbol-macro-to-env ((env cl-environment) name expansion)
  (setf (gethash name (variable-bindings env))
        (list :symbol-macro expansion)))

(defmethod add-variable-to-env ((env cl-environment) name x type)
  (setf (gethash name (variable-bindings env))
        (list type)))

(defmethod add-function-to-env ((env cl-environment) type name lambda-list body
                                &optional declarations doc-strings)
  (assert (member type '(:builtin-function :local-function :toplevel-function)))
  (setf (gethash name (function-bindings env))
        (list type name :lambda-list lambda-list :body body
              :declare declarations :doc doc-strings)))

(defun add-macro (name lambda)
  (add-macro-to-env *environment* name lambda))

(defmethod get-macro-function-in-env (name (env cl-environment))
  (let ((l (gethash name (function-bindings env))))
    (if l
        (when (eq (first l) :macro)
          ;; return nil for a local function binding, since it shadows
          ;; outer macros
          (cdr l))
        (get-macro-function-in-env name (parent-scope env)))))

(defmethod get-macro-function-in-env (name (env null))
  nil)


(defmethod get-symbol-macro-in-env (name (env cl-environment))
  (let ((l (gethash name (variable-bindings env))))
    (if l
        (when (eq (first l) :symbol-macro)
          ;; return nil for a local binding, since it shadows outer macros
          ;; -- CL uses a function for symbol macros, but not sure
          ;;    that is visible without macroexpand-hook, so just returning
          ;;    the expansion directly...
          #++(lambda (form expansion)
            (declare (ignore form expansion))
            (second l))
          ;; return 2nd value to indicate we actually have a symbol macro,
          ;;   even if it expands to NIL
          (values (second l) t))
        (get-symbol-macro-in-env name (parent-scope env)))))

(defmethod get-symbol-macro-in-env (name (env null))
  nil)

(defun get-macro-function (name)
  (let ((mf (get-macro-function-in-env name *environment*)))
    (when (and (consp mf) (not (second mf)))
      (setf (second mf) (compile 'nil (first mf))))
    (second mf)))

(defun get-symbol-macro (name)
  (get-symbol-macro-in-env name *environment*))

(defun add-symbol-macro (name expansion)
  (add-symbol-macro-to-env *environment* name expansion))

(defun add-variable (name x &optional (type :local-binding))
  (add-variable-to-env *environment* name x type))

(defun add-function (type name lambda-list body &optional declarations docs)
  (add-function-to-env *environment*
                       type name lambda-list body declarations docs))


(defclass cl-walker (walker)
  ())

(defmethod add-scope (env (walker cl-walker))
  (make-instance 'cl-environment :parent env))

(defmacro with-environment-scope ((walker) &body body)
  `(let ((*environment* (add-scope *environment* ,walker)))
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

(defwalker cl-walker (setq (&rest assignments))
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
  (with-environment-scope (walker)
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
  (with-environment-scope (walker)
    (loop for (name lambda-list . body) in bindings
          do (add-macro name
                        `(lambda (form env)
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

(defmacro with-lambda-list-vars ((walker lambda-list) &body body)
  `(with-environment-scope (,walker)
     (mapcar (lambda (a) (add-variable a nil))
             (lambda-list-vars ,lambda-list))
     ,@body))

(defun walk-function-body (walker lambda-list body)
  ;; fixme: not sure if this gets the scopes quite right when same
  ;; name appears multiple times
  ;; not sure it matters though, since anything that cares probably
  ;; has its own code walker?
  (let* ((declare (when (typep (car body) '(cons (member declare)))
                    (pop body)))
         (walked
           (with-environment-scope (walker)
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
     ,@(with-environment-scope (walker)
         (mapcar (lambda (a) (add-variable (if (consp a) (car a) a) nil))
                 bindings)
         (@@ body :declare t))))

(defwalker cl-walker (let* (&rest bindings) &rest body)
  ;; walk default values if any, and body
  (with-environment-scope (walker)
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
  (let ((macro (get-macro-function car)))
    (if macro
        (walk (funcall macro (list* car cdr) *environment*)
              walker)
        (call-next-method))))

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
  `(let ((*environment* *cl-environment*))
     (add-macro ,name
                (lambda (form env)
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

