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

(defwalker extract-functions (defun name lambda-list &body body+d)
  (multiple-value-bind (body declare doc)
      (alexandria:parse-body body+d :documentation t)
    #++(add-function :toplevel-function name lambda-list
                  (with-lambda-list-vars (walker lambda-list) (@@ body))
                  declare doc)
    (let ((f (process-type-declarations-for-scope
              (add-function name lambda-list
                            nil
                            :declarations declare :docs doc))))
      (setf (body f)
            (with-lambda-list-vars (f) (@@ body))))
    #++(call-next-method)
    nil))





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
  (when (typep (called-function form) 'global-function)
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
          #++(funcall *tree-shaker-type-hook* (interface-block b))
          (funcall *tree-shaker-type-hook* form)
          (funcall *tree-shaker-type-hook* form))))
  (call-next-method))

#++(defmethod walk-cons (car cdr (walker tree-shaker))
  (let ((ff (gethash car (function-bindings *environment*))))
    (when (eq (car ff) :toplevel-function)
      (funcall *tree-shaker-hook* car cdr)))
  (call-next-method))

#++
(defmethod walk :around (form (w tree-shaker))
  (call-next-method))

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
              #++(format t "edge from ~s to ~s~%" (name current-function)
                      (name name))
              (setf (gethash current-function (gethash name in-edges))
                    current-function))
          with *tree-shaker-type-hook*
            = (lambda (name)
                (pushnew name live-types))
          for root = (pop roots)
          while root
          do (setf (gethash root out-edges) (make-hash-table)
                   current-function root)
             (when (typep root 'global-function)
               (walk root (make-instance 'tree-shaker))))

    ;;; second pass: topo sort entries
    (setf roots (list root))
    ;(format t "add ~s~%" (name root))
    (push (name root) live)
    (loop for root = (pop roots)
          ;; remove edges from roots to children, then add
          ;; any children with no more incoming edges to roots
          while root
          do
             (alexandria:maphash-keys
              (lambda (k)
                (unless (eq root k)
                  (let ((in (gethash k in-edges) ))
                    ;; remove link from current root
                    (remhash root in)
                    ;; and add child to list of roots if there are no
                    ;; other callers
                    (when (zerop (hash-table-count in))
                    ;  (format t "add ~s~%" (name k))
                      (push (name k) live)
                      (push k roots)))))
              (gethash root out-edges))
             (remhash root out-edges))
    (values ;;(reverse (cons (name root) live))
     live
            (reverse live-types))))






;;;; main entry point for compiler


(defun compile-block (forms tree-shaker-root *current-shader-stage*
                      &key env print-as-main)
  (let ((*environment* (or env
                           (make-instance 'environment
                                          :parent glsl::*glsl-base-environment*))))
    (setf forms (walk (cons 'progn forms) (make-instance 'extract-functions)))
    (let ((*print-as-main* (when print-as-main
                             (get-function-binding print-as-main))))
      (multiple-value-bind (shaken shaken-types)
          (tree-shaker tree-shaker-root)
        (values
         forms
         *environment*
         shaken
         shaken-types
         (with-output-to-string (*standard-output*)
           (format t "#version 330~%")
           (pprint-glsl forms)
           (loop with dumped = (make-hash-table)
              for type in shaken-types
              for stage-binding = (stage-binding type)
              for interface-block = (when stage-binding
                                      (interface-block stage-binding))
              #+do   (format *debug-io* "add type? ~s (~s) ib ~s~%"
                             type (name type) interface-block)
              unless (or (internal type) (gethash interface-block dumped))
              do (pprint-glsl type)
              (when interface-block
                (setf (gethash interface-block dumped) t)))
           (loop for name in shaken
              for def = (gethash name (function-bindings *environment*))
              when (typep def 'global-function)
              do (pprint-glsl def)
              ))
         )))))


#++
(compile-block '((defun foo (a1 b1)
                   #++(+ a (* 3 (/ b)) 2))
)
               'foo)

#++
(multiple-value-list
 (compile-block '((defun foo (a b)
                    (+ a (* 3 (/ b)) 2))
                  (defparameter *foo-bar* (+ 123 4))
                  (defconstant +hoge-piyo+ 45.6)
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
                    (declare (:int e))
                    "do baz stuff"
                    #++(flet ((a (b)
                                (+ 1 b)))
                         (a 2))
                    (let ((e)
                          (f 1.0))
                      (when e
                        (foo 1 2)
                        (bar 2 3)
                        )
                      (if e
                          (calls-foo (foo e 1) (bar f 9))
                          (complicated (if f (ash f 1) (ash e -1)) (glsl::<< 4 +hoge-piyo+)
                                       :d 4)))))
                'main))

