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
(defclass extract-functions (glsl::glsl-walker)
  ())

;;; list of new functions (used to tell which functions were just
;;;  defined and need things like dependencies added and type
;;;  inference by later passes)
(defvar *new-function-definitions*)
(defvar *function-stages*)

(defwalker extract-functions (defun name lambda-list &body body+d)
  (format t "defun ~s~%" name)
  (multiple-value-bind (body declare doc)
      (alexandria:parse-body body+d :documentation t)
    (format t "declarations = ~s~%" declare)
    (let ((glsl::*current-function*
            (process-type-declarations-for-scope
             (add-function name lambda-list
                           nil
                           :declarations declare :docs doc)))
          (*function-stages* t))
      (clrhash (function-dependencies glsl::*current-function*))
      (when (boundp '*new-function-definitions*)
        (pushnew glsl::*current-function* *new-function-definitions*))
      (setf (body glsl::*current-function*)
            (with-lambda-list-vars (glsl::*current-function*) (@@ body)))
      (format t "defun ~s stages = ~s~%" name *function-stages*)
      ;; if *function-stages* is NIL, we got bindings that only exist
      ;; in disjoint sets of stages...
      (assert *function-stages*)
      (setf (valid-stages glsl::*current-function*)
            (alexandria:ensure-list *function-stages*))
      ;; fixme: move all these 'reset' things into add-function or something
      (clrhash (local-binding-type-data glsl::*current-function*))
      (clrhash (final-binding-type-cache glsl::*current-function*)))
    nil))


(defmethod check-stages (interface-binding)
  (let ((types
          (loop for (nil sb) on (stage-bindings interface-binding) by #'cddr
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
                x))))
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
              (if (eq t *function-stages*)
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

(defclass tree-shaker (glsl::glsl-walker)
  ())

(defmethod walk ((form function-call) (walker tree-shaker))
  (format t "ts: ~s -> ~s~%" form (name (called-function form)))
  (when (or (typep (called-function form) 'global-function)
            (typep (called-function form) 'unknown-function-binding))
    (funcall *tree-shaker-hook* (called-function form)))
  (call-next-method))

(defun walk-type (x)
  (when (typep x 'struct-type)
    (funcall *tree-shaker-type-hook* x)))

(defmethod walk ((form slot-access) (walker tree-shaker))
  (walk-type (binding form))
  (call-next-method))

(defmethod walk ((form variable-read) (walker tree-shaker))
  (walk-type (binding form))
  (call-next-method))

(defmethod walk ((form variable-write) (walker tree-shaker))
  (walk-type (binding form))
  (call-next-method))

(defmethod walk ((form binding) (walker tree-shaker))
  (walk-type (value-type form))
  (call-next-method))

(defmethod walk ((form constant-binding) (walker tree-shaker))
  (funcall *tree-shaker-type-hook* form)
  (call-next-method))


(defmethod walk ((form interface-binding) (walker tree-shaker))
  (let ((b (stage-binding form)))
    (when b
      (if (interface-block b)
          (funcall *tree-shaker-type-hook* form)
          (funcall *tree-shaker-type-hook* form))))
  (call-next-method))

;; todo: rewrite this to use pregenerated dependencies?
(defun tree-shaker (root)
  ;; we assume local functions have been inlined or extracted, and
  ;; names have been alpha converted as needed, etc already...
  (let* ((root (get-function-binding root))
         (in-edges (make-hash-table))
         (out-edges (make-hash-table))
         (roots (list root))
         (live ())
         (live-types ())
         (current-function root))
;;; first pass: walk the tree starting from root, and collect all edges
    (loop with *tree-shaker-hook*
            =
            (lambda (name)
              ;; store outgoing edges from current function being walked,
              ;; so we don't need to walk it again for next pass
              (setf (gethash name (gethash current-function out-edges)) name)
              ;; then if we haven't seen the function being called before,
              ;; add it to list to be walked
              (unless (gethash name in-edges)
                (push name roots)
                (setf (gethash name in-edges) (make-hash-table)))
              ;; finally, add an incoming edge from current function to
              ;; function being called
              (setf (gethash current-function (gethash name in-edges))
                    current-function))
          with *tree-shaker-type-hook* = (lambda (name)
                                           (pushnew name live-types))
          for root = (pop roots)
          while root
          do (setf (gethash root out-edges) (make-hash-table)
                   current-function root)
             (when (typep root 'global-function)
               (walk root (make-instance 'tree-shaker))))

;;; second pass: topo sort entries
    (setf roots (list root))
    (push (name root) live)
    (loop for root = (pop roots)
          ;; remove edges from roots to children, then add
          ;; any children with no more incoming edges to roots
          while root
          do
             (alexandria:maphash-keys
              (lambda (k)
                (unless (eq root k)
                  (let ((in (gethash k in-edges)))
                    ;; remove link from current root
                    (remhash root in)
                    ;; and add child to list of roots if there are no
                    ;; other callers
                    (when (zerop (hash-table-count in))
                      (push (name k) live)
                      (push k roots)))))
              (gethash root out-edges))
             (remhash root out-edges))
    (values live (reverse live-types))))


;; add dependencies to specified function-binding-function
;;  also add function as a dependent to any functions it depends on?
(defun update-dependencies (function)
  ;; reuse tree-shaker walker, find all functions called and add to list
  ;;
  (format t "update deps ~s~%" (name function))
  (let* ((current-function (if (symbolp function)
                               (get-function-binding function)
                               function))
         (*tree-shaker-hook*
           (lambda (f)
             (assert (or (typep f 'function-binding-function)
                         (typep f 'unknown-function-binding)))
             ;; fixme: add a proper "unknown function" warning,
             ;;  and somehow mark to skip type inference step
             (format t "add dep ~s calls ~s~%" (name current-function)
                     (name f))
             (setf (gethash f (function-dependencies current-function))
                   f)
             (setf (gethash current-function (function-dependents f))
                   current-function))))
    (walk current-function (make-instance 'tree-shaker))))

(defclass update-calls (glsl::glsl-walker)
  ((modified :initarg :modified :reader modified)))

(defmethod walk ((form function-call) (walker update-calls))
  (let ((*environment* (argument-environment form)))
    (setf (arguments form)
          (mapcar (lambda (x)
                    (format t "update ~s~%" x)
                    (walk x walker))
                  (funcall (expander (called-function form))
                           (raw-arguments form)))))
  (call-next-method))

;;;; main entry point for compiler
(defun compile-block (forms tree-shaker-root *current-shader-stage*
                      &key env print-as-main)
  (let* ((*environment* (or env
                            (make-instance 'environment
                                           :parent glsl::*glsl-base-environment*)))
         (*global-environment* *environment*)
         (*new-function-definitions* nil))
    (setf forms (walk (cons 'progn forms) (make-instance 'extract-functions)))
    (loop for f in *new-function-definitions*
          do (update-dependencies f))
    ;; if any functions' lambda list was changed, recompile any calls to those
    ;; functions in their dependents
    (let* ((changed-signatures (remove-if-not #'function-signature-changed
                                              *new-function-definitions*))
           (deps (make-hash-table))
           (update-calls (make-instance 'update-calls
                                        :modified (alexandria:alist-hash-table
                                                   (mapcar (lambda (a)
                                                             (cons a nil))
                                                           changed-signatures)))))
      (loop for i in changed-signatures
            do (maphash (lambda (k v) (setf (gethash k deps) v))
                        (function-dependents i))
               (setf (old-lambda-list i)
                     (lambda-list i)))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (walk k update-calls))
               deps))

    (print forms)
    (let ((*print-as-main* (when print-as-main
                             (get-function-binding print-as-main)))
          (inferred-types))
      (multiple-value-bind (shaken shaken-types)
          (tree-shaker tree-shaker-root)
        (values
         forms
         *environment*
         shaken
         shaken-types
         (infer-modified-functions *new-function-definitions*)
         (setf inferred-types
               (finalize-inference (get-function-binding tree-shaker-root)))
         (with-output-to-string (*standard-output*)
           (format t "#version 440~%")
           (pprint-glsl forms)
           (loop with dumped = (make-hash-table)
                 for type in shaken-types
                 for stage-binding = (stage-binding type)
                 for interface-block = (when stage-binding
                                         (interface-block stage-binding))
                 unless (or (internal type) (gethash interface-block dumped))
                   do (pprint-glsl type)
                      (when interface-block
                        (setf (gethash interface-block dumped) t)))
           (loop for name in shaken
                 for def = (gethash name (function-bindings *environment*))
                 for overloads = (gethash def inferred-types)
                 when (typep def 'global-function)
                   do (assert overloads)
                      (loop for overload in overloads
                            for *binding-types*
                              = (gethash overload
                                         (final-binding-type-cache def))
                            do (assert *binding-types*)
                               (pprint-glsl def)))))))))


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
                          (complicated (if f (glsl::<< f 1) (glsl::>> e 1)) (glsl::<< 4 +hoge-piyo+)
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
(glsl::generate-stage :fragment 'skybox-shaders::fragment)
#++
(print (glsl::generate-stage :fragment '3bgl-mesh-shaders::fragment))
#++
(print (glsl::generate-stage :geometry '3bgl-mesh-shaders::tsd-geometry))

