(in-package #:3bgl-shaders)

;;; in types.lisp
;; concrete-type
;; array-type
;; aggregate-type
;; struct-type
;; interface-type
;; set-type
;;

;; inference:
;;   walk tree, build list of set-types for expressions, add constraints
;;   to unification worklist

;; constraints:
;;   alias: not sure we need this, just always use same type object
;;          for aliased type, or use 'equiv' field so they can all
;;          link to a single type?

;;   ftype-set: set of combinations of types for a function application
;;     for example a function might allow (int int) -> int
;;       and (float float) -> float
;;     contains a list of set-types for each arg, a set-type for ret
;;        might also store a copy of the set of types for each arg + ret
;;        to use for checking for changes?
;;        alternately, probably want a modified flag for each arg + ret?
;;          (to use flag, linked types would need link back in order to
;;           know when to toggle flag. we can't store it in the set-type,
;;           since multiple types might depend on it, so can't know when
;;           to turn it off)
;;        possibly as a compromise, increment a counter in set-type
;;          when modified, and store last known value in ftype-set, since
;;          it would be faster to compare an int than a list?
;;        -- rather than counter, use length, since that should
;;           be monotonically decreasing?
;;        -- actually, need links from set-types back to ftype-sets anyway
;;           so we can add other ftype-sets to worklist when a set-type is
;;           modified

;;     and a set of ftypes (each containing a list of concrete types for args
;;     and 1 for ret)

;; unification:
;;    get a ftype-set from worklist
;;      loop through modified args + ret
;;        filter out any ftypes that don't match updated arg/ret types
;;      loop through all args + ret
;;        calculate set of types for that position in ftype list
;;        if different from current value
;;          update types in that set-type
;;          mark all other constraints linked to that type as modified
;;          add any constraints that were not previously marked to worklist
;;



;;; (defun foo (a b)
;;;   (let ((c (+ a 2.0)))
;;;     (return (texture-1d b (.xy c))))
;;;  FOO = A B -> RET
;;;  A = set T
;;;  B = set T
;;;  RET = set T
;;;  F1 (+ a 2.0) = C1 application (A 2.0)
;;;     gentype gentype -> gentype
;;;     mat mat -> mat
;;;     vec f -> vec
;;;     f vec -> vec
;;;     mat f -> mat
;;;     f mat -> mat
;;;  C = F1
;;;  F2 (.xy c) = C2 application (C)
;;;     vec4 -> vec2
;;;     vec3 -> vec2
;;;     vec2 -> vec2
;;;  F3 (texture-2d B F2) = C3 application (B F2)
;;;     sampler2d vec2 -> vec4
;;;  F4 (return F3) = C4 equality RET = F3 = F4
;;;
;;; worklist = C1 C2 C3 C4
;;;   C1 = F1 T 2.0 -> T
;;;     float float -> float
;;;     vec float -> vec
;;;     mat float -> met
;;;     A => (set float vec* mat*)
;;;     C = F1 => (set float vec* mat) -> C2 already modified
;;;   C2 = F2 (float vec* mat) -> T
;;;     F2 => vec2
;;;     C => (set vec2 vec3 vec4) -> C1 + modified
;;;   C3 (B F2) = (T vec2)
;;;     sampler2d vec2 -> vec4
;;;     B => sampler2d
;;;     F3 => vec4
;;;   C4 eq (ret f3 f4) T vec4 T
;;;     F4 = RET => vec4
;;;   C1 = (A 2.0) -> F1 = (float vec* mat*) 2.0 -> vec2 vec3 vec4
;;;     vec2/3/4 float -> vec2/3/4
;;;     A => vec2 vec3 vec4
;;; FOO = vec2/vec3/vec4 sampler2D -> vec4

(defclass constrained-type ()
  ((constraints :initform nil :accessor constraints)
   ;;
   (equality-constraint :initform nil :accessor equality-constraint))
)

(defclass set-type (constrained-type)
  ((length)
   (types :initarg :types :accessor types) ;; list? of concrete types
))

(defclass any-type (constrained-type)
  ())


;; having a separate constant-type instead of a set-type with 1 type seems
;; to complicate things more than it helps...
#++
(defclass constant-type (constrained-type)
  ((type :initarg :type :accessor ctype)))

(defclass constraint ()
  ((modified :initform nil :accessor modified)))

(defclass equality-constraint (constraint)
  ((types)) ;; list of SET-TYPEs
)

(defclass function-application (constraint)
  ((argument-types :initarg :argument-types :reader argument-types) ;; list of set-types
   (return-type :initarg :return-type :reader return-type) ;; SET-TYPE?
   (function-types :initarg :function-types :accessor function-types) ;; modifiable list of ftype, each is a list of concrete arg types + concrete ret type
   (name :initarg :name :accessor name) ;; for debugging
   ))


(defvar *inference-worklist*)

(defclass infer-build-constraints (glsl::glsl-walker)
  ())

(defun copy-type (type)
  (if (consp type)
      (copy-list type)
      type))

(defun set-type (types #+&key instance)
  (typecase types
    ((eql t)
     (make-instance 'any-type))
    (symbol
     #++(make-instance 'constant-type :type types)
     (set-type (list types)))
    (concrete-type
     ;; fixme: combine the inference types with other type stuff properly
     (set-type (list (name types))))
    (list
     (let ((set (make-hash-table)))
       (loop for i in types do (setf (gethash i set) i))
       (make-instance 'set-type :types set)))))

(defmethod walk ((form function-call) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (let* ((called (called-function form))
         (function-type (function-type called)))
    (when (typep called 'unknown-function-binding)
     (error "got call to unknown function ~s during type inference"
            (name called)))
    (if (eql function-type t)
        (prog1 t (format t "T*->T function call"))
        (let ((return-type (set-type (if (consp function-type)
                                         (mapcar 'second function-type)
                                         function-type)))
              (argument-types (mapcar (lambda (a) (walk a walker))
                                      (arguments form))))
          (when argument-types
            (let ((constraint
                    (make-instance 'function-application
                                   :argument-types argument-types
                                   :function-types (copy-type function-type)
                                   :return-type return-type
                                   :name (name called))))
              (loop for i in (cons return-type argument-types)
                    do (push constraint (constraints i)))
              (setf (modified constraint) t)
              (push constraint *inference-worklist*)))
          (print return-type)))))

(defmethod walk ((form variable-read) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (print (value-type (binding form)))
)

(defmethod walk ((form function-argument) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (print (value-type form))
)

(defmethod walk ((form integer) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (print (set-type (list :int :uint :float))))

(defmethod walk ((form float) (walker infer-build-constraints))
  (format t "infer ~s =~%" form)
  (print  (set-type :float)))

(defmethod walk ((form global-function) (walker infer-build-constraints))
  (format t "infer ~s  (~s)=~%" form (body form))
  (loop for binding in (bindings form)
        for declared-type in (declared-types form)
        if (typep declared-type '(cons (eql T) (cons unsigned-byte)))
          do (setf (value-type binding)
                   (value-type (elt (bindings form) (second declared-type))))
        else do (setf (value-type binding)
                      (set-type declared-type)))
  ;; fixme: add a constraint if return type isn't T
  (assert (eq t (return-type form)))

  (print (loop for a in (body form)
               for ret = (walk a walker)
               do (format t "@ global-function / ~s~%" ret)
               finally (return (setf (return-type form) ret)))))


(defmethod walk (form (walker infer-build-constraints))
  (format t "unhandled form ~s~%" form)
  (when (next-method-p)
    (call-next-method))
)

(defmacro defmethod2 (name (a b) &body body)
  ;; define methods on NAME with arguments A B and B A
  `(progn
     (defmethod ,name (,a ,b) ,@body)
     (defmethod ,name (,b ,a) ,@body)))

(defmethod2 unify ((a set-type) (b set-type))
  (maphash ))

(defmethod update-constraint ((constraint function-application))
  ;; fixme: keep track of which args were modified and only process them...
  ;; fixme: rearrange data structures to avoid copies here?
  (let ((removed 0)
        (removed2 0))
    ;; no point in processing a T*->T function, since it won't
    ;; restrict any of the arguments or return type, so it shouldn't
    ;; have a constraint in the first place...
    (assert (not (eql t (function-types constraint))))
    (format t "update constraint (~s ftypes)~%" (length (function-types constraint)))
    ;; loop through function types, keep ones that match the argument/ret types
    (setf (function-types constraint)
          (loop for ftype in (function-types constraint)
                for (args ret) = ftype
                do (format t "  check constraint ~s:" ftype)
                when (prin1
                      (and
                       (gethash ret (types (return-type constraint)))
                       (loop for ftype-arg in args
                             for arg in (argument-types constraint)
                             always (or (typep arg 'any-type)
                                        ;; possibly should add a TYPES
                                        ;; reader to constant-types to
                                        ;; simplify this?
                                        (progn #+if (typep arg 'constant-type)
                                               #++(eql ftype-arg (ctype arg))
                                            (gethash ftype-arg (types arg)))))))
                  collect ftype
                else do (incf removed)
                do (terpri)))
    (format t " -> (~s ftypes)~%" (length (function-types constraint)))
    ;; loop through argument types, set all to NIL
    (loop for arg in (cons (return-type constraint)
                           (argument-types constraint))
          unless (or (typep arg 'any-type)
                     #++(typep arg 'constant-type))
            do (maphash (lambda (k v)
                          (declare (ignore v))
                          (setf (gethash k (types arg)) nil))
                        (types arg)))
    ;; expand any-type args/ret
    (flet ((hash-list (l)
             (let ((h (make-hash-table)))
               (loop for i in l do (setf (gethash i h) nil))
               h)))
      (when (typep (return-type constraint) 'any-type)
        (change-class (return-type constraint)
                      'set-type
                      :types (hash-list (mapcar 'second
                                                (function-types constraint)))))
      (loop for arg in (argument-types constraint)
            for i from 0
            when (typep arg 'any-type)
              do (change-class arg 'set-type
                               :types (hash-list
                                       (mapcar (lambda (a) (nth i (first a)))
                                               (function-types constraint))))))

    ;; loop through function types again, turn back on the valid argument types
    (loop for (nil ret) in (function-types constraint)
          do (setf (gethash ret (types (return-type constraint)))
                   ret))
    (loop for ftype in (function-types constraint)
          for (args ret) = ftype
          do (assert (nth-value 1 (gethash ret (types (return-type constraint)))))
             (loop for ftype-arg in args
                   for arg in (argument-types constraint)
                   ;unless (typep arg 'constant-type)
                     do (assert (nth-value 1 (gethash ftype-arg (types arg))))
                        (setf (gethash ftype-arg (types arg))
                              ftype-arg)
                        (incf removed2)))
    ;; (possibly loop through arg types again and remove NIL values?
    (format t "updated constraint, removed ~s ftypes, ~s arg types~%" removed removed2)
    (assert (or (plusp removed) (plusp removed2)))
    )

)


(defun infer (function)
  (format t "~&infer ~s (~s ~s)~%" (name function)
          (function-type function)
          (old-function-type function))
  (let* ((*inference-worklist* nil)
         (ret (walk function (make-instance 'infer-build-constraints))))
    ;; leaves are at end of list, so we process them before interior nodes
    ;; (should be correct either way, but wastes effort doing interior first)
    (setf *inference-worklist* (nreverse *inference-worklist*))
    (loop for constraint = (pop *inference-worklist*)
          while constraint
          do (update-constraint constraint))
    ret
)
)
(defun infer-modified-functions (functions)
  ;; find dependents of modified functions, and add to a list in
  ;; dependency order
  (format t "infer-modified-functions ~s~%" functions)
  (let ((in (make-hash-table))
        (out (make-hash-table))
        (leaves nil))
    ;; build local copy of dependency graph including modified
    ;; functions and all dependents
    (labels ((walk (f &optional up)

               (format t "walk ~s~@[ @ ~s~] ~%" (name f) (and up (name up)))
               (unless (gethash f in)
                 (setf (gethash f in) (make-hash-table)
                       (gethash f out) (make-hash-table))
                 (maphash (lambda (k v)
                            (declare (ignore v))
                            (setf (gethash k (gethash f out)) t)
                            (walk k f))
                          (function-dependents f)))
               (when up
                 (format t "add ~s <- ~s~%" (name f) (name up))
                 (setf (gethash up (gethash f in)) t))))
      (mapcar #'walk functions))
    (loop for (k . v) in (alexandria:hash-table-alist in)
          do (format t "~s <-~{ ~s~}~%" (name k)
                     (mapcar 'name (alexandria:hash-table-keys v))))

    (loop for (k . v) in (alexandria:hash-table-alist out)
          do (format t "~s ->~{ ~s~}~%" (name k)
                     (mapcar 'name (alexandria:hash-table-keys v))))
    ;; topo sort function list
    (loop for c = 0
          while (plusp (hash-table-count out))
          do (maphash (lambda (k v)
                        ;; find leaves
                        (when (zerop (hash-table-count v))
                          (incf c)
                          (format t "~&leaf ~s~%" (name k))
                          (push k leaves)
                          ;; remove them from table
                          (remhash k out)
                          ;; and from dependents of their dependencies
                          (maphash (lambda (k2 v2)
                                     (declare (ignore v2))
                                     (format t "  remove ~s from ~s~%"
                                             (name k) (name k2))
                                     (remhash k (gethash k2 out)))
                                   (gethash k in))
))
                      out)
          when (zerop c) do (error "?~s ~s ~s" in out leaves))
    (print (mapcar 'name leaves))
    ;; run type inference on list
    (loop for function in leaves
          when (function-type-changed function)
            collect (infer function)
          do
               (when (function-type-changed function)
                 ;; mark dependents as needing inference
                 (loop for k being the hash-keys of (function-dependents
                                                     function)
                       do (setf (function-type k) t))
                 (setf (old-function-type function)
                       (function-type function))))
    ;;
    ))

#++
(multiple-value-list
 (compile-block '((defun h (a)
                    ;(declare (:int a))
                    (+ a 2.0)))
                   'h
                   :vertex))

#++
(multiple-value-list
 (compile-block '((defun h (a)
                    ;(declare (:int a))
                    (< a 2)))
                   'h
                   :vertex))
