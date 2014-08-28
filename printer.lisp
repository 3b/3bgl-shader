(in-package #:3bgl-shaders)

(defparameter *in-expression* nil)

;; hack to rename a specific function as 'main' when printing, so we
;; can define a bunch of shaders for different stages (or different
;; features) in same package without worrying about which can be named
;; MAIN.
(defparameter *print-as-main* nil)

;; we want to use call-next-method to print mixins, so using a custom
;; print function rather than pprint-dispatch stuff...

(defgeneric %print (object stream))
(defmethod %print (object stream)
  ;; do nothing by default
  )

;; but we want to hook into normal printing, so we add to the pprint
;; dispatch table and call %print from there
(defparameter *pprint-glsl* (copy-pprint-dispatch nil))



(defun %translate-name (x &key lc-underscore)
  ;; todo: lookup name in environment to allow for specifying a translation
  (unless (stringp x)
    (setf x (string x)))
  (with-output-to-string (s)
    ;; fixme: smarter translation, filter more characters, etc
    (if (char= #\+ (char x 0) (char x (1- (length x))))
        (with-standard-io-syntax
          (format s "~:@(~a~)" (substitute #\_ #\- (remove #\+ x))))
        (if lc-underscore
            (format s "~(~a~)" (substitute #\_ #\- (remove #\+ x)))
            (loop with uc = 0
               for c across (string x)
               do (case c
                    (#\* (incf uc 1))
                    (#\- (incf uc 1))
                    (#\+
                     (incf uc 1)
                     (format s "_"))
                    (#\% (format s "_"))
                    (t (if (plusp uc)
                           (format s "~:@(~c~)" c)
                           (format s "~(~c~)" c))
                     (setf uc (max 0 (1- uc))))))))))

(defmethod translate-name (x)
  (%translate-name x))

(defmethod translate-name ((x binding))
  (or (glsl-name x) (%translate-name (name x))))

(defmethod translate-name ((x function-binding))
  (if (eq x *print-as-main*)
      "main"
      (or (glsl-name x) (%translate-name (name x)))))

(defmethod translate-name ((x slot-access))
  (format nil "~a.~a" (binding x)
          (translate-slot-name (field x) (binding x))))

(defmethod translate-name ((x swizzle-access))
  (format nil "~a.~a" (binding x)
          (translate-slot-name (field x) (binding x))))

(defmethod translate-name ((x array-access))
  (format nil "~a[~a]" (binding x)
          (index x)))

(defmethod translate-slot-name (x b)
  (%translate-name x))


(defmethod translate-slot-name (x (b variable-read))
  (or (translate-slot-name x (binding b))
      (%translate-name x)))

(defmethod translate-slot-name (x (b array-access))
  (or (translate-slot-name x (binding b))
      (%translate-name x)))

(defmethod translate-slot-name (x (b interface-binding))
  (or (translate-slot-name x (stage-binding b))
      (%translate-name x)))

(defmethod translate-slot-name (x (b interface-stage-binding))
  (when (or (interface-block b) (typep (binding b) 'bindings))
    (let ((b2 (bindings (or (interface-block b) (binding b)))))
      (loop for binding in b2
            when (eq (name binding) x)
              do (return-from translate-slot-name (or (glsl-name binding)
                                                      (%translate-name x)))))
    (%translate-name x)))

#++
(defmethod translate-name ((x interface-binding))
  (translate-name (binding x)))

(defmethod translate-name ((x interface-stage-binding))
  (translate-name (or (interface-block x) (binding x))))

(defmethod translate-name ((x generic-type))
  (or (glsl-name x) (%translate-name (name (get-equiv-type x)))))

(defmethod translate-name ((x array-type))
  (translate-name (base-type x)))

(defmethod translate-name ((x variable-read))
  (translate-name (binding x)))


(defmethod translate-type (type)
  (string type)
  #++(error "don't know how to compile type ~s" type))

(defmethod translate-type ((type any-type))
  ;; fixme: just for debugging, should be an error once things are working properly
  "T")

(defmethod translate-type ((type concrete-type))
  (glsl-name type))

(defmethod translate-type ((type constrained-type))
  (let ((types))
    (maphash (lambda (k v) (when v (push k types))) (types type))
    (if (= 1 (length types))
        (glsl-name (car types))
     (mapcar #'glsl-name types))))

(defmethod translate-type ((type generic-type))
  (let ((e (get-equiv-type type)))
    (if (eq type e)
        (string type) ;;?
        (translate-type e))))


(defmacro assert-statement ()
  `(when *in-expression*
     (error "trying to print statement in expression context")))

#++(set-pprint-dispatch 'symbol
                     (lambda (s o)
                       (format s "~a" (translate-name o)))
                     0
                     *pprint-glsl*)
(defmethod %print ((o symbol) s)
  (format s "~a" (translate-name o)))

(set-pprint-dispatch 'double-float
                     (lambda (s o)
                       (let ((*read-default-float-format* 'double-float))
                         ;; fixme: how should these be printed?
                         (format s "~f"  o)))
                     0
                     *pprint-glsl*)


 ;; print uny cons without a higher-priority printer as a normal function call
#++
(set-pprint-dispatch 'cons
                     (lambda (s o)
                       (let ((*in-expression* t))
                         (format s "~a~<(~;~@{~:_~a~#[~:;, ~]~}~;)~:>" (car o) (cdr o))))
                     0
                     *pprint-glsl*)

;;; printers for calls to "internal-function" variants
(defparameter *internal-function-printers* (make-hash-table))

(defmacro defprinti ((form &rest args) () &body body)
  (alexandria:with-gensyms (stream object)
    `(setf (gethash ',form *internal-function-printers*)
           (lambda (,stream ,object)
             ;; OBJECT is a FUNCTION-CALL instance, (called-function object)
             ;; should be a INTERNAL-FUNCTION, with NAME eq FORM
             (let ((*standard-output* ,stream))
               (destructuring-bind (,@args) ,object
                 ,@body))))
    #++`(set-pprint-dispatch
      '(cons (member ,form))
      (lambda (,stream ,object)
        (let ((*standard-output* ,stream))
          (destructuring-bind (,@args) (cdr ,object)
            ,@body)))
      1 *pprint-glsl*)))


(defmacro defprint-binop (op c-op 0-arg 1-arg)
  `(defprinti (,op &rest args) ()
     (let ((*in-expression* t))
       (case (length args)
         (0 ,(or 0-arg `(error ,(format nil "no arguments to ~a ?" op))))
         (1 ,(or
              (if (eq 1-arg t)
                  `(format t "~a" (car args))
                  1-arg)
              `(format t ,(format nil "(~a~~a)" c-op) (car args))))
         (t (format t ,(format nil "(~~{~~a~~^ ~~#[~~:;~a ~~]~~})" c-op) args))))))

;;; fixme: probably shouldn't be allowing 0/1 arg versions of these that
;;;        use some specific number, since it should have been accounted for
;;;        before type inference?
(defprint-binop - "-" nil nil)
(defprint-binop + "+" 0.0 t)
(defprint-binop * "*" 1.0 t)
(defprint-binop / "/" 1.0 (format t "(1.0 / ~a)" (car args)))
(defprint-binop or "||" 0 t)
(defprint-binop and "&&" 1 t) ;; should this be #xffffffff instead of 1 for t?
(defprint-binop glsl:^^ "^^" 0 t)
(defprint-binop logior "|" 0 t)
(defprint-binop logand "&" #xffffffff t) ;; fixme: how many bits is -1?
(defprint-binop logxor "^" 0 t)



(defprinti (1- x) ()
  (let ((*in-expression* t))
    (format t "~a" `(,x - 1))))
(defprinti (1+ x) ()
  (let ((*in-expression* t))
    (format t "~a" `(,x + 1))))
(defprinti (not x) ()
  (let ((*in-expression* t))
    (format t "~a" `(! ,x))))

;; only handling binary versions of compare ops for now,
;; can expand multi arg versions in earlier pass if needed
(macrolet ((compares (&rest ops)
             `(progn
                ,@(loop for (op c-op) in ops
                        collect `(defprinti (,op a b) ()
                                   (let ((*in-expression* t))
                                     (format t ,(format nil"(~~a ~a ~~a)" c-op)
                                             a b)))))))
  (compares (= "==")
            (/= "!=")
            (< "<")
            (> ">")
            (<= "<=")
            (>= ">=")))


(defprinti (ash i c) ()
  (cond
    ((numberp c)
     (let ((*in-expression* t))
       (format t "(~a ~a ~a)" i (if (plusp c) "<<" ">>") (abs c))))
    (t (error "tried to print ASH with non-constant shift?"))))

(defprinti (glsl::<< i c) ()
 (let ((*in-expression* t))
   (format t "(~a << ~a)" i c)))

(defprinti (glsl::>> i c) ()
 (let ((*in-expression* t))
   (format t "(~a >> ~a)" i c)))

(defprinti (return x) ()
  (assert-statement)
  (let ((*in-expression* t))
    (format t "return ~a" x))) 



(defmacro defprint (type (object) &body body)
  (alexandria:with-gensyms (stream)
    `(progn
       (defmethod %print ((,object ,type) *standard-output*)
         ,@body)
       (set-pprint-dispatch
        ',type
        (lambda (,stream ,object)
          (%print ,object ,stream))
        1 *pprint-glsl*))))

(defprint initialized-binding (o)
  (assert-statement)
  (let ((*in-expression* t))
    (format t "~{~(~a ~)~}~@[~a ~]~a~@[ = ~a~]"
            (qualifiers o)
            (translate-type (or  (and (boundp '*binding-types*)
                                     (gethash o *binding-types*))
                                (value-type o)))
            (translate-name o)
            (initial-value-form o))))

(defprint binding (o)
  (assert-statement)
  (let ((*in-expression* t))
    (format t "~{~a ~}~@[~a ~]~a"
            (qualifiers o)
            (translate-type (or (and (boundp '*binding-types*)
                                     (gethash o *binding-types*))
                                (value-type o)))
            (translate-name o))))

(defprint slot-access (o)
  (format t "~a" (translate-name o)))

(defprint swizzle-access (o)
  (format t "~a" (translate-name o)))

(defprint array-access (o)
  (format t "~a" (translate-name o)))


#++
(defprint (:var name &key init qualifiers type) ()
  (assert-statement)
  (let ((*in-expression* t))
    (with-standard-io-syntax
      (format *debug-io* ":var ~s ~s ~s ~s~%" name init qualifiers type))
    (format t "~{~a ~}~@[~a ~]~a~@[ = ~a~]" qualifiers type name init)))

#++(defprint (:var name &key init qualifiers type) ()
  (assert-statement)
  (let ((*in-expression* t))
    (with-standard-io-syntax
      (format *debug-io* ":var ~s ~s ~s ~s~%" name init qualifiers type))
    (format t "~{~a ~}~@[a ~]~a~@[ = ~a~]" qualifiers type name init)))


#++
(defprint (defun name (&rest lambda-list) &body body) ()
  (assert-statement)
  (let ((ret (if (consp name) (second name) "void"))
        (name (if (consp name) (first name) name)))
    (format t "--~a ~a ~<(~;~@{~:_~a~#[~:;, ~]~}~;)~:> {~%" ret name
            lambda-list)
    (format t "~a" (cons 'progn body))
    (format t "}~%"))
  )


(defprint global-function (o)
  (assert-statement)
  ;; fixme: clean this layout stuff up...
  ;; if function is "main", check for extra layout qualifiers
  (when (and (string= (translate-name o) "main") (layout-qualifiers o))
    (maphash (lambda (k v)
               (format t "layout(~a~{~^,~a=~a~}) ~a;~%"
                       (%translate-name (car v) :lc-underscore t)
                       (loop for (a b) on (cdr v) by #'cddr
                             collect (%translate-name a :lc-underscore t)
                             collect b)
                       (translate-name k))
               )
             (layout-qualifiers o))
    )

  ;; print function def
  (format t "~a ~a ~<(~;~@{~:_~a~#[~:;, ~]~}~;)~:> {~%"
          (or (translate-type (or (gethash :return *binding-types*)
                                  (value-type o))) "void")
          (translate-name o)
          (bindings o))
  (call-next-method)
  (format t "~&}~%"))

(defprint function-call (o)
  (let ((f (called-function o))
        (args (arguments o)))
    (typecase f
      (internal-function
       (funcall (gethash (name f) *internal-function-printers*
                         (lambda (s args)
                           (let ((*in-expression* t))
                             (format s "~a~<(~;~@{~:_~a~#[~:;, ~]~}~;)~:>"
                                     (or (translate-name f) (name f))
                                     args))
                           ))
                *standard-output* args))
      (t
       #++(with-standard-io-syntax
         (format *debug-io* "call args ~a~%" args))
       (let ((*in-expression* t))
         (format t "~a~<(~;~@{~:_~a~#[~:;, ~]~}~;)~:>"
                 (translate-name f) args))))))


(defprint progn-body (o)
  (if *in-expression*
      (format t "~<(~@;~@{~a~^,~})~:>" (body o))
      (format t "~{~a~^;~%~}" (body o))))

(defprint implicit-progn (o)
  (if *in-expression*
      (call-next-method)
      (format t "~<  ~@;~@{~a;~^~%~}~:>" (body o))))

(defprint binding-scope (o)
  (assert-statement)
  ;; fixme: avoid extra {} in LET at top level of a function
  ;; (bind a special when inside LET scope, clear it inside scopes
  ;;  that add a binding scope (like FOR, DEFUN, etc?))
  (format t "{~%~<  ~@;~@{~a;~^~%~}~:>~%" (bindings o))
  (call-next-method)
  (format t "~&}"))

(defprint variable-read (o)
  (format t "~a" (translate-name (binding o))))

(defprint variable-write (o)
  (if *in-expression*
      (format t "(~a = ~a)"
              (translate-name (binding o))
              (let ((*in-expression* t)) (value o)))
      (format t "~a = ~a"
              (translate-name (binding o))
              (let ((*in-expression* t)) (value o)))))

(defprint if-form (o)
  (let ((cond (test-form o))
        (then (then-form o))
        (else (else-form o)))
    #++(with-standard-io-syntax
      (format *debug-io* "if ~a ~a ~a~%" cond then else))
    (if *in-expression*
        (format t "(~a?~a:~a)" cond then else)
        (progn
          (let ((*in-expression* t))
            (format t "if (~a) {~%" cond))
          (format t "~<  ~@;~a;~:>" (list then))
          (when else
            (format t "~&} else {~%")
            (format t "~<  ~@;~a;~:>" (list else)))
          (format t "~&}")))))


(defprint for-loop (o)
  (let ((initialize (init-forms o))
        (term (condition-forms o))
        (step (step-forms o)))
    (if *in-expression*
        (error "can't expand 'for' loop in expression context")
        (progn
          (let ((*in-expression* t))
            (format t "for (~{~a~^,~};~{~a~^,~};~{~a~^,~}) {~%"
                    initialize term step))
          (call-next-method)
          (format t "~&}")))))


(defmethod array-suffix (x)
  nil)


(defmethod array-suffix ((x array-type))
  (typecase (array-size x)
    (number (format nil "[~a]" (array-size x)))
    (null nil)
    (t "[]")))

(defmethod array-suffix ((x interface-binding))
  (typecase (array-size x)
    (number (format nil "[~a]" (array-size x)))
    (null nil)
    (t "[]")))

(defmethod array-suffix ((x interface-stage-binding))
  (typecase (array-size x)
    (number (format nil "[~a]" (array-size x)))
    (null nil)
    (t "[]")))

(defprint interface-binding (o)
  #++(format *debug-io* "~a ~s~%" (translate-name o) (internal o))
  (let ((b (stage-binding o)))
    (cond
      ((or (interface-block b) (typep (binding b) 'bindings))
       (format t "~a ~a {~%~<  ~@;~@{~a;~^~%~}~:>~%}~@[ ~a~]~@[~a~];~%"
               (translate-name (interface-qualifier b))
               (translate-name b)
               (bindings (or (interface-block b) (binding b)))
               (unless (interface-block b) (translate-name o))
               (array-suffix  b)))
      (t
       (format t "~@[layout(~(~{~a = ~a~^,~}~)) ~]~a ~a ~a~@[~a~];~%"
               (layout-qualifier b)
               (translate-name (interface-qualifier b))
               (translate-name (value-type b))
               (translate-name o)
               (array-suffix (value-type b)))))))

(defprint constant-binding (o)
  (call-next-method)
  (format t ";"))

(defun pprint-glsl (form)
  #+=(%print form *standard-output*)
  (let* ((*print-pprint-dispatch* *pprint-glsl*)
         (*print-pretty* t)
         (old-debug *debugger-hook*)
         (*debugger-hook* (lambda (&rest r)
                            (with-standard-io-syntax
                              (apply old-debug r)))))
    (format t "~a~%" form)))
