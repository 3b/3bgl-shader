(in-package #:3bgl-shaders)

(defparameter *in-expression* nil)

;; hack to rename a specific function as 'main' when printing, so we
;; can define a bunch of shaders for different stages (or different
;; features) in same package without worrying about which can be named
;; MAIN.
(defparameter *print-as-main* nil)

;; we need to rename variables in some cases to avoid conflicts so
;; track which variables are live to avoid creating other conflicts in
;; the process
;; glsl name -> lisp symbol
(defparameter *live-variables* (make-hash-table :test 'equal))

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
  (let ((n (or (glsl-name x) (%translate-name (name x)))))
    (when (or (conflicts x)
              (and (gethash n *live-variables*)
                   (not (eq (name (gethash n *live-variables*)) (name x)))))
      (loop for i from 2 below 1000
            for rn = (format nil "~a_~a" n i)
            for c = (gethash rn *live-variables*)
            while (and c (not (eq c x)))
            finally (setf n rn)))
    n))

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

(defmethod translate-type ((type ref-type))
  (translate-type (get-equiv-type type)))

(defmethod translate-type ((type concrete-type))
  (glsl-name type))

(defmethod translate-type ((type constrained-type))
  (let ((types))
    (maphash (lambda (k v) (when v (push k types))) (types type))
    (if (= 1 (length types))
        (translate-name (car types))
        (mapcar #'translate-name types))))

(defmethod translate-type ((type generic-type))
  (let ((e (get-equiv-type type)))
    (if (eq type e)
        (string type) ;;?
        (translate-type e))))

(defmethod translate-type ((type array-type))
  (translate-type (base-type
                   (or (and (boundp '*binding-types*)
                            (gethash type *binding-types*))
                       (value-type type)))))

(defmethod translate-type ((type struct-type))
  (or (glsl-name type) (%translate-name (name type))))



(defmacro assert-statement ()
  `(when *in-expression*
     (with-standard-io-syntax
       (error "trying to print statement in expression context"))))

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

(defmacro defprinti ((form &rest args) (&optional (call (gensym))) &body body)
  (alexandria:with-gensyms (stream object)
    `(setf (gethash ',form *internal-function-printers*)
           (lambda (,stream ,object &key ((:call ,call) nil))
             (declare (ignorable ,call))
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
(defprint-binop 3bgl-glsl:^^ "^^" 0 t)
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

(defprinti (3bgl-glsl::incf x &optional y) ()
  (let ((*in-expression* t))
    (if y
        (format t "(~a+=~a)" x y)
        (format t "(~a++)" x))))
(defprinti (3bgl-glsl::decf x &optional y) ()
  (let ((*in-expression* t))
    (if y
        (format t "(~a-=~a)" x y)
        (format t "(~a--)" x))))

(defprinti (3bgl-glsl::++ x) ()
  (let ((*in-expression* t))
    (format t "(++~a)" x)))
(defprinti (3bgl-glsl::-- x) ()
  (let ((*in-expression* t))
    (format t "(--~a)" x)))

(defmethod integral-type ((type concrete-type))
  (and (scalar/vector-set type)
       (member (name (aref (scalar/vector-set type) 1))
               '(:int :uint))))

(defmethod integral-type ((type constrained-type))
  (some #'integral-type (alexandria:hash-table-keys (types type))))

(defprinti (mod a b) (call)
  (let ((*in-expression* t)
        (type (gethash call *binding-types*)))
    #++(with-standard-io-syntax
      (break "mod" a b call *binding-types*))
    (with-standard-io-syntax
      (assert type))
    (if (integral-type (first type))
        (format t "(~a % ~a)" a b)
        (format t "mod(~a, ~a)" a b))))


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

(defprinti (zerop x) (call)
  (let ((*in-expression* t)
        (type (gethash call *binding-types*)))
    (with-standard-io-syntax
      (assert type))
    (if (integral-type (second type))
        (format t "(0 == ~a)" x)
        (format t "(0.0 == ~a)" x))))

(defprinti (plusp x) (call)
  (let ((*in-expression* t)
        (type (gethash call *binding-types*)))
    (with-standard-io-syntax
      (assert type))
    (if (integral-type (second type))
        (format t "(~a > 0)" x)
        (format t "(~a > 0.0)" x))))

(defprinti (minusp x) (call)
  (let ((*in-expression* t)
        (type (gethash call *binding-types*)))
    (with-standard-io-syntax
      (assert type))
    (if (integral-type (second type))
        (format t "(~a < 0)" x)
        (format t "(~a < 0.0)" x))))

(defprinti (ash i c) ()
  (cond
    ((numberp c)
     (let ((*in-expression* t))
       (format t "(~a ~a ~a)" i (if (plusp c) "<<" ">>") (abs c))))
    (t (error "tried to print ASH with non-constant shift?"))))

(defprinti (3bgl-glsl::<< i c) ()
 (let ((*in-expression* t))
   (format t "(~a << ~a)" i c)))

(defprinti (3bgl-glsl::>> i c) ()
 (let ((*in-expression* t))
   (format t "(~a >> ~a)" i c)))

(defprinti (return x) ()
  (assert-statement)
  (let ((*in-expression* t))
    (format t "return ~a" x)))

(defprinti (values &optional x) ()
  (when x
    (let ((*in-expression* t))
      (format t "~a" x))))

(defprinti (3bgl-glsl:discard) ()
  (assert-statement)
  (format t "discard"))


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

(defun vector->{} (x)
  (typecase x
    (array-initialization
     (with-output-to-string (s)
       (with-standard-io-syntax (break "foo1" x))
       (format s "{")
       (loop for (a more) on (arguments x)
             do (%print a s)
             when more do (format s ", "))
       (format s "}")))
    (vector
     (with-output-to-string (s)
       (with-standard-io-syntax (break "foo2" x))
       (format s "{")
       (loop for a across x
             for i from 0
             do (%print a s)
             while (< i (length x))
             do (format s ", "))
       (format s "}"))
     )
    (t x)))

(defprint array-initialization (o)
  (format t "{~{~a~^, ~}}" (arguments o))
  #++(format t "(~{~a~^, ~})" (arguments o)))

(defparameter *interface-qualifier-order*
  ;; ordering is relaxed in 4.2, but still need to get centroid before
  ;; in/out as well, so might as well sort anyway
  (alexandria:plist-hash-table
   '(:precise 1
     :invariant 2
     :smooth 5 :flat 5 :noperspective 5 ;; interpolation
     :no-perspective 5
     ;; 4.x, unspecified order, so just picking one...
     :coherent 7 :volatile 7 :restrict 7 :readonly 7 :writeonly 7
     :read-only 7 :write-only 7
     ;; centroid etc immediately before in/out (part of storage qual in spec)
     :centroid 9 :patch 9 :sample 9
     :in 10 :const 10 :out 10 :attribute 10 :uniform 10 ;; storage qualifier
     :buffer 10 :shared 10
     :varying 10                     ;; deprecated
     :lowp 15 :mediump 15 :highp 15  ;; precision
     )))

(defun sort-interface-qualifiers (q)
  (flet ((c (a b)
           ;; possibly should default to something for unknown
           ;; qualifiers, but probably would be wrong so just error
           ;; here
           (< (gethash a *interface-qualifier-order*)
              (gethash b *interface-qualifier-order*))))
    (sort (copy-list (alexandria:ensure-list q)) #'c)))

(defun translate-interface-qualifiers (q)
  (flet ((tx (n)
           (string-downcase (remove #\- (symbol-name n)))))
    (mapcar #'tx (sort-interface-qualifiers q))))


(defprint initialized-binding (o)
  (assert-statement)
  (let ((*in-expression* t))
    (if (typep (value-type o) 'array-type)
        (format t "~{~(~a ~)~}~@[~a ~]~a[~a]~@[ = ~a~]"
                ;; might need type for literal initializers? if so, figure
                ;; out how to distinguish them...
                #++"~{~(~a ~)~}~@[~a ~]~a[~a]~@[ = ~3:*~a[]~2*~a~]"
                (translate-interface-qualifiers (qualifiers o))
                (translate-type (base-type
                                 (or (and (boundp '*binding-types*)
                                          (gethash o *binding-types*))
                                     (value-type o))))
                (translate-name o)
                (array-size (value-type o))
                (initial-value-form o))
        (format t "~{~(~a ~)~}~@[~a ~]~a~@[ = ~a~]"
                (translate-interface-qualifiers (qualifiers o))
                (translate-type (or (and (boundp '*binding-types*)
                                         (gethash o *binding-types*))
                                    (value-type o)))
                (translate-name o)
                (initial-value-form o)))))

(defprint binding (o)
  (assert-statement)
  (let ((*in-expression* t))
    (format t "~{~a ~}~@[~a ~]~a~@[~a~]"
            (translate-interface-qualifiers (qualifiers o))
            (translate-type (or (and (boundp '*binding-types*)
                                     (gethash o *binding-types*))
                                (value-type o)))
            (translate-name o)
            (array-suffix (or (and (boundp '*binding-types*)
                                   (gethash o *binding-types*))
                              (value-type o))))))

(defprint slot-access (o)
  (format t "~a" (translate-name o)))

(defprint swizzle-access (o)
  (format t "~a" (translate-name o)))

(defprint array-access (o)
  (format t "~a" (translate-name o)))

(defun print-main-layout-qualifiers (q)
  (maphash (lambda (k v)
             (format t "layout(~{~@[~a=~]~@[~a~^,~]~}) ~a;~%"
                     (loop for (a b) on v by #'cddr
                           ;; allow nil -> X or X -> X
                           ;; to mean single element without =
                           for single = (or (not a) (eq a b))
                           collect (unless single
                                     (%translate-name a :lc-underscore t))
                           collect (if (and single b)
                                       (%translate-name b :lc-underscore t)
                                       b))
                     (translate-name k)))
           q))

(defprint global-function (o)
  (assert-statement)
  ;; fixme: clean this layout stuff up...
  ;; if function is "main", check for extra layout qualifiers
  (when (and (string= (translate-name o) "main") (layout-qualifiers o)
             ;; compute layout qualifiers need printed earlier so
             ;; other functions can use gl_WorkGroupSize etc
             (not (eql *current-shader-stage* :compute)))
    (print-main-layout-qualifiers (layout-qualifiers o)))

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
                         (lambda (s args &key &allow-other-keys)
                           (let ((*in-expression* t))
                             (format s "~a~<(~;~@{~:_~a~#[~:;, ~]~}~;)~:>"
                                     (or (translate-name f) (name f))
                                     args))))
                *standard-output* args :call o))
      (t
       (let ((*in-expression* t))
         (format t "~a~<(~;~@{~:_~a~#[~:;, ~]~}~;)~:>"
                 (translate-name f) args))))))


(defprint progn-body (o)
  (if *in-expression*
      (format t "~<(~;~@{~a~^,~})~:>" (body o))
      (format t "~{~a~^;~%~}" (body o))))

(defprint implicit-progn (o)
  (if *in-expression*
      (call-next-method)
      (format t "~<  ~@;~@{~a;~^~%~}~:>" (body o))))

(defprint binding-scope (o)
  (with-standard-io-syntax (assert-statement))
  ;; fixme: avoid extra {} in LET at top level of a function
  ;; (bind a special when inside LET scope, clear it inside scopes
  ;;  that add a binding scope (like FOR, DEFUN, etc?))
  (let ((shadowed nil))
    (loop for binding in (bindings o)
          for glsl = (translate-name binding)
          for old = (gethash glsl *live-variables*)
          do (if old
                 (push (cons glsl old) shadowed)
                 (push (cons glsl nil) shadowed))
             (setf (gethash glsl *live-variables*)
                   binding))
    (format t "{~%~<  ~@;~@{~a;~^~%~}~:>~%" (bindings o))
    (call-next-method)
    (loop for (g . l) in shadowed
          do (if l
                 (setf (gethash g *live-variables*) l)
                 (remhash g *live-variables*))))
  (format t "~&}"))

(defprint variable-read (o)
  (format t "~a" (translate-name (binding o))))

(defprint variable-write (o)
  (if *in-expression*
      (let ((*in-expression* t))
        (format t "(~a = ~a)"
                (translate-name (binding o))
                (value o)))
      (let ((*in-expression* t))
        (format t "~a = ~a"
                (translate-name (binding o))
                (value o)))))

(defprint if-form (o)
  (let ((cond (test-form o))
        (then (then-form o))
        (else (else-form o)))
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

(defprint struct-type (o)
  (format t "struct ~a {" (translate-name o))
  (format t "~%~<  ~@;~@{~a;~^~%~}~:>~%" (bindings o))
  (format t "};~%"))


(defmethod array-suffix (x)
  nil)

(defmethod array-suffix ((x array-type))
  (typecase (array-size x)
    (number (format nil "[~a]" (array-size x)))
    (null nil)
    ((or symbol binding) (format nil "[~a]" (translate-name (array-size x))))
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
  (let ((b (stage-binding o)))
    (format t "~@[layout(~(~{~@[~a = ~]~a~^,~}~)) ~]"
            (loop for (a b) on (layout-qualifier b) by #'cddr
                  when b
                    append (if (eq b t)
                               ;; :X t -> x
                               (list nil a)
                               ;; NIL :X -> x
                               ;; :x y -> x=y
                               (list (and (not (eq a b))  a)
                                     b))))
    (cond
      ((typep (binding b) 'struct-type)
       (format t "~{~a~^ ~} ~a ~a~@[~a~]"
               (translate-interface-qualifiers (interface-qualifier b))
               (translate-name
                (name (or (interface-block b) (binding b))))
               (translate-name o)
               (array-suffix (value-type b))))
      ((or (interface-block b) (typep (binding b) 'bindings))
       (format t "~{~a ~}~a {~%~<  ~@;~@{~a;~^~%~}~:>~%}~@[ ~a~]~@[~a~]"
               (translate-interface-qualifiers (interface-qualifier b))
               (translate-name b)
               (bindings (or (interface-block b) (binding b)))
               (unless (interface-block b) (translate-name o))
               (array-suffix  b)))
      (t
       (format t "~{~a~^ ~} ~a ~a~@[~a~]"
               (translate-interface-qualifiers (interface-qualifier b))
               (translate-name (value-type b))
               (translate-name o)
               (array-suffix (value-type b)))))
    (let ((d (default b)))
      (cond
        ((not d))
        ((symbolp d)
         (format t " = ~a" (translate-name d)))
        (t
         (format t " = ~a" d))))
    (format t ";~%")))

(defprint constant-binding (o)
  (call-next-method)
  (format t ";"))

(defun pprint-glsl (form)
  (let* ((*print-pprint-dispatch* *pprint-glsl*)
         (*print-pretty* t)
         (*print-circle* nil)
         (old-debug *debugger-hook*)
         (*debugger-hook* (lambda (&rest r)
                            (with-standard-io-syntax
                              (apply old-debug r)))))
    (format t "~a~%" form)))
