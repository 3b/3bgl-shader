(in-package #:3bgl-shaders)


;;; passes
;;; +base pass (expand macros, symbol macros, etc)
;;     (possibly combined with next pass, since it probably inherits from
;;      walker that does expansion already)
;;; +extract top-level definitions
;;; +inline local functions?
;;;  alpha conversion?
;;;    (including resolving comflicts between fn/var/etc namespaces?)

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
    (add-function :toplevel-function name lambda-list
                  (with-lambda-list-vars (walker lambda-list) (@@ body))
                  declare doc)
    #++(call-next-method)
    nil))





;;;; tree-shaker
;;;   given an entry point, return a list of all functions called by that
;;;   entry point, in reverse dependency order (so main entry point last)
(defparameter *tree-shaker-live* nil)
(defparameter *tree-shaker-depth* 0)
(defparameter *tree-shaker-roots* nil)
(defparameter *tree-shaker-hook* (lambda (&rest r) (declare (ignore r))))

(defclass tree-shaker (glsl::glsl-walker)
  ())

(defmethod walk-cons (car cdr (walker tree-shaker))
  (let ((ff (gethash car (function-bindings *environment*))))
    (when (eq (car ff) :toplevel-function)
      (funcall *tree-shaker-hook* car cdr)))
  (call-next-method))

(defmethod walk :around (form (w tree-shaker))
  (call-next-method))

(defun tree-shaker (root)
  ;; we assume local functions have been inlined or extracted, and
  ;; names have been alpha converted as needed, etc already...
  (let ((in-edges (make-hash-table))
        (out-edges (make-hash-table))
        (roots (list root))
        (live (list root)))
    ;;; first pass: walk the tree starting from root, and collect all edges
    (loop with *tree-shaker-hook* =
          (lambda (car cdr)
            (declare (ignore cdr))
            ;; store outgoing edges from current function being walked,
            ;; so we don't need to walk it again for next pass
            (setf (gethash car (gethash root out-edges)) car)
            ;; then if we haven't seen the function being called before,
            ;; add it to list to be walked
            (unless (gethash car in-edges)
              (push car roots)
              (setf (gethash car in-edges) (make-hash-table)))
            ;; finally, add an incoming edge from current function to
            ;; function being called
            (setf (gethash root (gethash car in-edges)) root))
          for root = (pop roots)
          while root
          do (setf (gethash root out-edges) (make-hash-table))
             (destructuring-bind (type name &key body lambda-list
                                  &allow-other-keys)
                 (gethash root (function-bindings *environment*))
               ;; just wrapping it in a fake defun form rather than
               ;;    walking the body by hand for now...
               (when (eq :toplevel-function type)
                 (walk `(defun ,name ,lambda-list ,@body)
                       (make-instance 'tree-shaker)))))

    ;;; second pass: topo sort entries
    (setf roots (list root))
    (loop for root = (pop roots)
          ;; remove edges from roots to children, then add
          ;; any children with no mor eincoming edges to roots
          while root
          do
             (alexandria:maphash-keys
              (lambda (k)
                (let ((in (gethash k in-edges) ))
                  ;; remove link from current root
                  (remhash root in)
                  ;; and add child to list of roots if there are no
                  ;; other callers
                  (when (zerop (hash-table-count in))
                    (push k live)
                    (push k roots))))
              (gethash root out-edges))
             (remhash root out-edges))
    live)
)






;;;; main entry point for compiler


(defun compile-block (forms tree-shaker-root)
  (let ((*environment* (make-instance 'cl-environment
                                      :parent glsl::*glsl-base-environment*)))
    (setf forms (walk (cons 'progn forms) (make-instance 'extract-functions)))
    (let ((shaken (tree-shaker tree-shaker-root)))
      (values
       forms
       *environment*
       shaken
       (with-output-to-string (*standard-output*)
         (pprint-glsl (remove 'nil forms))
         (loop for name in shaken
               for def = (gethash name (function-bindings *environment*))
               when (eq (car def) :toplevel-function)
               do (pprint-glsl def)
               ))
       ))))



#++
(compile-block '((defun foo (a b)
                   (+ a (* 3 (/ b)) 2))
                 (defparameter *foo-bar* 123)
                 (defconstant +hoge-piyo+ 45.6)
                 (defmacro bar (c d)
                   `(- ,c ,(+ d 10)))
                 (defun not-called (g)
                   (foo 1 2))
                 (defun calls-foo (a b)
                   (foo a b))
                 (defun main ()
                   (declare (:int e))
                   "do baz stuff"
                   (:var e :type :int)
                   (:var f :type :float)
                   (if e
                       (calls-foo (foo e 1) (bar f 9))
                       (foo (if f (ash f 1) 2) (ash 4 g)))))
               'main)
