(in-package #:3bgl-shaders)

;;; passes
;;; +base pass (expand macros, symbol macros, etc)
;;     (possibly combined with next pass, since it probably inherits from
;;      walker that does expansion already)
;;; +extract top-level definitions
;;; +inline local functions?
;;     has to happen after alpha conversion, since free variables
;;     in the local function bodies need to refer to bindings in the
;;     scope when the function was defined, as opposed to when it was
;;     called
;;;  alpha conversion?
;;;    (including resolving conflicts between fn/var/etc namespaces?)
;;     if possible, it would be nice to leave names unchanged, and
;;       rely on on {} for shadowing lexical scopes, but if so, we need
;;       to watch out for inlined local closures

;;;  convert into some less-cl form?
;;;    tree of objects (or just plists)?
;;;  constant folding?
;;;  (partial) type inference for secondary functions
;;     for anything that isn't the main function, we allow parameters
;;     and return types to be not fully determined, and later try to
;;     generate overloads for any variants actually used after tree shaker
;;;  type inference on main function
;;; +tree shaker
;;     possibly before any type inference? depends on if we are keeping
;;     secondary functions around for multiple compiles (which we probably
;;     want to do, for libs and such)
;;;  generate specialized versions of any partially types functions
;;;  generate code


;;; first pass: expand macros, extract function definitions into environments
;;; (should basically just leave variable definitions/initialization?)
(defclass extract-functions (3bgl-glsl::glsl-walker)
  ())

;;; list of new functions (used to tell which functions were just
;;;  defined and need things like dependencies added and type
;;;  inference by later passes)
(defvar *new-function-definitions*)
(defvar *new-type-definitions*)
(defvar *new-global-definitions*)
(defvar *function-stages*)

(defwalker extract-functions (defun name lambda-list &body body+d)
  (multiple-value-bind (body declare doc)
      (alexandria:parse-body body+d :documentation t)
    (let* ((3bgl-glsl::*current-function*
             (process-type-declarations-for-scope
              (add-function name lambda-list
                            nil
                            :declarations declare :docs doc)))
           (*function-stages* (valid-stages 3bgl-glsl::*current-function*)))
      (clrhash (bindings-used-by 3bgl-glsl::*current-function*))
      (when (boundp '*new-function-definitions*)
        (pushnew 3bgl-glsl::*current-function* *new-function-definitions*))
      (setf (body 3bgl-glsl::*current-function*)
            (with-lambda-list-vars (3bgl-glsl::*current-function*) (@@ body)))
      ;; if *function-stages* is NIL, we got bindings that only exist
      ;; in disjoint sets of stages...
      (assert *function-stages*)
      (setf (valid-stages 3bgl-glsl::*current-function*)
            (alexandria:ensure-list *function-stages*)))
    nil))

(macrolet ((track-globals (&rest forms)
             `(progn
                ,@(loop for form in forms
                        collect
                        `(defwalker extract-functions (,form name &rest rest)
                           (declare (ignore rest))
                           (prog1
                               (call-next-method)
                             (when (boundp '*new-global-definitions*)
                               (when (consp name)
                                 (setf name (car name)))
                               (assert (get-variable-binding name))
                               (pushnew (list name (get-variable-binding name))
                                        *new-global-definitions*))))))))
  (track-globals defconstant defparameter
                 3bgl-glsl:defconstant 3bgl-glsl::%defconstant
                 3bgl-glsl:attribute 3bgl-glsl:uniform
                 3bgl-glsl:input 3bgl-glsl:output
                 3bgl-glsl:bind-interface))

(macrolet ((track-types (&rest forms)
             `(progn
                ,@(loop for form in forms
                        collect
                        `(defwalker extract-functions (,form name &rest rest)
                           (declare (ignore rest))
                           (prog1
                               (call-next-method)
                             (when (boundp '*new-type-definitions*)
                               (pushnew (list name (get-type-binding name))
                                        *new-type-definitions*))))))))
  (track-types defstruct))




(defmethod check-stages (interface-binding)
  (let ((types
          (loop for (nil sb) on (stage-bindings interface-binding) by #'cddr
                ;; don't check for conflicts in IN/OUT for now
                for .iq = (interface-qualifier sb)
                for iq = (if (consp .iq)
                             (remove-if (lambda (a) (member a '(:in :out)))
                                        .iq)
                             (if (member .iq '(:in :out))
                                 nil
                                 .iq))
                when iq
                  collect (binding sb))))
    (unless (or (every (lambda (a) (typep a 'interface-type))
                       types)
                (every (lambda (a) (eq a (car types)))
                       (cdr types)))
      (error "conflicting types for interface binding ~s : ~{~s~^ ~}"
             (name interface-binding)
             (remove-duplicates(mapcar 'name types))))))

(defmethod check-slot-stages (slot-access)
  (labels ((get-interface-bindings (x)
             (etypecase x
               ((or slot-access variable-read variable-write array-access)
                (get-interface-bindings (binding x)))
               (interface-binding
                x)
               (local-variable
                (return-from check-slot-stages t)))))
    (let* ((interface-bindings (get-interface-bindings slot-access))
           (types
             (loop for (nil sb) on (stage-bindings interface-bindings) by #'cddr
                   for b = (binding sb)
                   for st = (if (typep b 'interface-type)
                                (find (field slot-access)
                                      (bindings b)
                                      :key #'name)
                                b)
                   when st
                     collect (value-type st))))
      (unless (every (lambda (a) (eq a (car types)))
                     (cdr types))
        (error "conflicting types for slot ~s.~s : ~{~s~^ ~}"
               (name interface-bindings) (field slot-access)
               (remove-duplicates(mapcar 'name types)))))))

(defmethod walk :around (form (walker extract-functions))
  (let ((r (call-next-method)))
    (when (typep r 'slot-access)
      (check-slot-stages r))
    (when (or (typep r 'variable-read)
              (typep r 'variable-write))
      (when (typep (binding r) 'interface-binding)
        (check-stages (binding r))
        (let ((stage-bindings (stage-bindings (binding r))))
          (unless (getf stage-bindings t)
            (let ((stages (loop for s in stage-bindings by #'cddr
                                collect s)))
              (if (or (eq t *function-stages*)
                      (equalp '(t) *function-stages*))
                  (setf *function-stages* stages)
                  (setf *function-stages*
                        (intersection *function-stages* stages))))))))
    r))

;;;; tree-shaker
;;;   given an entry point, return a list of all functions called by that
;;;   entry point, in reverse dependency order (so main entry point last)
;;
;; we also need to declare any aggregate types we use, so we need to
;; mark variable usage, then map those back to structure/interface
;; types and dump those as well
;(defparameter *tree-shaker-live* nil)
;(defparameter *tree-shaker-depth* 0)
;(defparameter *tree-shaker-roots* nil)
(defparameter *tree-shaker-hook* (lambda (&rest r) (declare (ignore r))))
(defparameter *tree-shaker-type-hook* (lambda (&rest r) (declare (ignore r))))
(defparameter *tree-shaker-current-object* nil)
;; fixme: rename this stuff, since tree-shaker doesn't use it anymore
(defclass tree-shaker ()
  ())

(defmethod walk ((form cons) (walker tree-shaker))
  (flet ((w (x)
           (walk x walker)))
    (map nil #'w form)))

(defmethod walk ((form function-call) (walker tree-shaker))
  (when (or (typep (called-function form) 'global-function)
            (typep (called-function form) 'unknown-function-binding))
    (funcall *tree-shaker-hook* (called-function form)))
  (call-next-method))

(defmethod walk ((form (eql t)) (walker tree-shaker))
  ;; for unspecified declared types
  form)
(defmethod walk ((form (eql :*)) (walker tree-shaker))
  ;; for unspecified array size
  form)

(defmethod walk ((form concrete-type) (walker tree-shaker))
  form)

(defmethod walk ((form slot-access) (walker tree-shaker))
  (walk (binding form) walker)
  (call-next-method))

(defmethod walk ((form variable-read) (walker tree-shaker))
  (walk (binding form) walker)
  (call-next-method))

(defmethod walk ((form variable-write) (walker tree-shaker))
  (walk (binding form) walker)
  (call-next-method))

(defmethod walk ((form swizzle-access) (walker tree-shaker))
  (walk (binding form) walker)
  (call-next-method))

(defmethod walk ((form local-variable) (walker tree-shaker))
  (walk (value-type form) walker)
  (walk (declared-type form) walker)
  (call-next-method))

(defmethod walk ((form binding) (walker tree-shaker))
  (walk (declared-type form) walker)
  (walk (value-type form) walker)
  (call-next-method))

(defmethod walk ((form constant-binding) (walker tree-shaker))
  (funcall *tree-shaker-type-hook* form)
  (call-next-method))

(defmethod walk ((form interface-binding) (walker tree-shaker))
  (funcall *tree-shaker-type-hook* form)
  (let ((*tree-shaker-current-object* form))
    (walk (stage-binding form) walker))
  (call-next-method))

(defmethod walk ((form interface-stage-binding) (walker tree-shaker))
  (walk (binding form) walker)
  (call-next-method))

(defmethod walk ((form struct-type) (walker tree-shaker))
  (funcall *tree-shaker-type-hook* form)
  (let ((*tree-shaker-current-object* form))
    (walk (bindings form) walker)))

(defmethod walk ((form array-type) (walker tree-shaker))
  (walk (base-type form) walker)
  (walk (array-size form) walker))

(defmethod walk ((form for-loop) (walker tree-shaker))
  (walk (condition-forms form) walker)
  (walk (step-forms form) walker)
  (call-next-method))

;; todo: rewrite this to use pregenerated dependencies?
(defun tree-shaker (root)
  ;; we assume local functions have been inlined or extracted, and
  ;; names have been alpha converted as needed, etc already...
  (let* ((root (get-function-binding root)))
    (assert root)
    (reverse (topo-sort-dependencies root #'bindings-used-by))))


;; add dependencies to specified function-binding-function
;;  also add function as a dependent to any functions it depends on?
(defun update-dependencies (form)
  ;; reuse tree-shaker walker, find all functions called and add to list
  ;;
  (assert (not (symbolp form)))
  (let* ((*tree-shaker-current-object* form)
         (*tree-shaker-hook*
           (lambda (f)
             (when (and (not (eq f *tree-shaker-current-object*))
                        (typep f 'binding-with-dependencies))
               (setf (gethash f (bindings-used-by *tree-shaker-current-object*))
                     f)
               (setf (gethash *tree-shaker-current-object* (bindings-using f))
                     *tree-shaker-current-object*))))
         (*tree-shaker-type-hook*
           (lambda (f)
             (when (and (not (eq f *tree-shaker-current-object*))
                        (typep f 'binding-with-dependencies))
               (setf (gethash f (bindings-used-by *tree-shaker-current-object*))
                     f)
               (setf (gethash *tree-shaker-current-object* (bindings-using f))
                     *tree-shaker-current-object*)))))
    (walk *tree-shaker-current-object* (make-instance 'tree-shaker))))

(defclass update-calls (3bgl-glsl::glsl-walker)
  ((modified :initarg :modified :reader modified)))

(defmethod walk ((form function-call) (walker update-calls))
  (let ((*environment* (argument-environment form)))
    (setf (arguments form)
          (mapcar (lambda (x)
                    (walk x walker))
                  (funcall (expander (called-function form))
                           (raw-arguments form)))))
  (call-next-method))


#++
(multiple-value-list
  (compile-block '((defun foo (a1 b1)
                       (+ a (* 3 (/ b)) 2)))
                   'foo
                   :vertex))

#++
(multiple-value-list
  (compile-block '((input position :vec4 :location 0)
                   (defun foo (a1 b1)
                        (+ a (* 3 (/ b)) 2)))
                   'foo
                   :vertex))


#++
(multiple-value-list
 (compile-block '((defun foo (a b)
                    (+ a (* 3 (/ b)) 2))
                  (defparameter *foo-bar* (+ 123 4))
                  (defconstant +hoge-piyo+ 45)
                  (defmacro bar (c d)
                    `(- ,c ,(+ d 10)))
                  (defun not-called (g)
                    (foo 1 2))
                  (defun calls-foo (a b)
                    (foo a b))
                  (defun complicated (a &optional (b 1.0) &key (c 2 cp)
                                                            (d 3))
                    (if cp (+ a b c) (+ a b)))
                  (defun main ()
                    "do baz stuff"
                    #++(flet ((a (b)
                                (+ 1 b)))
                         (a 2))
                    (let ((e)
                          (f 1))
                      (when e
                        (foo 1 2)
                        (bar 2 3))
                      (if e
                          (calls-foo (foo e 1) (bar f 9))
                          (complicated (if f (3bgl-glsl::<< f 1) (3bgl-glsl::>> e 1)) (3bgl-glsl::<< 4 +hoge-piyo+)
                                       :d 4)))))
                'main
                :vertex))

#++
(multiple-value-list
 (compile-block '((defun a () (let ((aa 1.0))

                                (+ aa (b aa) (b 1) (c) (d))))
                  (defun a2 (a) (+ (b a) (c)))
                  (defun b (bb) (+ (e) (f) bb))
                  (defun c () (b 2))
                  (defun d () (f))
                  (defun e () (d))
                  (defun f () (+ (g) (h)))
                  (defun g () 1)
                  (defun h () 2))
                'a
                :vertex))


#++
(3bgl-glsl::generate-stage :fragment 'skybox-shaders::fragment)
#++
(print (3bgl-glsl::generate-stage :fragment '3bgl-mesh-shaders::fragment))
#++
(print (3bgl-glsl::generate-stage :geometry '3bgl-mesh-shaders::tsd-geometry))

