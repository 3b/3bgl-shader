(in-package #:3bgl-shaders)

;; calling generate-stage while compile-form is running might see an
;; inconsistent stage, and is reasonably likely when compiling a whole
;; file if calling code doesn't wait a while before calling
;; generate-stage. Doesn't seem to be any reaonable way to determine
;; how long to wait, so adding a lock so at worst we just get
;; redundant shader recompiles instead of errors.
(defparameter *compiler-lock* (bordeaux-threads:make-lock
                               "3bgl-shaders:compile-form"))

(defvar *modified-function-hook* nil
  "list of functions to call when shader functions are
modified. Passed a list of names of functions that have been
modified.  May be called multiple times for same function if a whole
file using the glsl:defun macro is recompiled, so probably should
store names and only update shader programs at next frame rather
than updating programs directly from hook function.")

;; compiler entry points

;; first pass of compilation for one or more forms
;; (expand macros, partial type inference, update dependencies, etc)
(defun compile-form (form)
  "Run first passes of compilation on specified form. (Wrap with PROGN
to process multiple forms). Calls functions in
*MODIFIED-FUNCTION-HOOK* with names of any functions whose definitions
are possibly affected by compiling FORM (for example functions that
call a function defined/updated by FORM, and the (re)defined function
itself). "
  (let ((modified-function-names nil))
    (bordeaux-threads:with-lock-held (*compiler-lock*)
        (glsl::with-package-environment ()
       (let ((*new-function-definitions* nil)
             (*new-type-definitions* nil)
             (*new-global-definitions* nil))
         ;; 'compile' forms
         (walk form (make-instance 'extract-functions))
         ;; update dependencies for any (re)defined functions
         (loop for f in *new-function-definitions*
               do (update-dependencies f))
         (loop for (nil f) in *new-global-definitions*
               do (update-dependencies f))
         ;; if any functions' lambda list was changed, recompile any
         ;; calls to those functions in their dependents
         (let* ((changed-signatures (remove-if-not #'function-signature-changed
                                                   *new-function-definitions*))
                (deps (make-hash-table))
                (update-calls (make-instance 'update-calls
                                             :modified
                                             (alexandria:alist-hash-table
                                              (mapcar (lambda (a)
                                                        (cons a nil))
                                                      changed-signatures)))))
           (loop for i in changed-signatures
                 do (maphash (lambda (k v) (setf (gethash k deps) v))
                             (bindings-using i))
                    (setf (old-lambda-list i)
                          (lambda-list i)))
           (maphash (lambda (k v)
                      (declare (ignore v))
                      (walk k update-calls))
                    deps))
         (let ((modified-deps (make-hash-table)))
           (loop for (nil i) in *new-type-definitions*
                 for deps = (print (bindings-using i))
                 do (loop for i being the hash-keys of deps
                          when (typep (print i) 'global-function)
                            do (setf (gethash i modified-deps) t)))
           (loop for (nil i) in *new-global-definitions*
                 for deps = (print (bindings-using i))
                 do (loop for i being the hash-keys of deps
                          when (print (typep (print i) 'global-function))
                            do (setf (gethash i modified-deps) t)))
           (loop for i in *new-function-definitions*
                 do (setf (gethash i modified-deps) t))
           (format t "deps = ~s~%" (mapcar 'name (alexandria:hash-table-keys modified-deps)))
           (when (plusp (hash-table-count modified-deps))
             (let ((modified (infer-modified-functions
                              (alexandria:hash-table-keys modified-deps))))
              (assert modified)
              (loop for f in modified
                    do (pushnew (name f) modified-function-names)))))

         (format t "modified functions: ~s~%" modified-function-names)
         (format t "modified types: ~s~%" *new-type-definitions*)
         (format t "modified globals: ~s~%" *new-global-definitions*))))
    ;; call hook outside lock in case it tries to call generate-stage
    (map nil (lambda (a) (funcall a modified-function-names))
         *modified-function-hook*)
    nil))

;; final pass of compilation
;; finish type inference for concrete types, generate glsl
(defun generate-stage (stage main &key (version 450))
  "Generate GLSL shader for specified STAGE, using function named by
MAIN as glsl 'main' function. ROOT and all functions/variables/etc it
depends on should already have been successfully compiled with
COMPILE-FORM. STAGE is :VERTEX, :FRAGMENT, :GEOMETRY, :TESS-EVAL,
:TESS-CONTROL, or :COMPUTE. VERSION specifies the value of the version
pragma in generated shader, but doesn't otherwise affect generated
code currently. "
  (bordeaux-threads:with-lock-held (*compiler-lock*)
   (glsl::with-package-environment (main)
     (let* ((*print-as-main* (get-function-binding main))
            (*current-shader-stage* stage))
       (let ((shaken (tree-shaker main)))
         (let ((inferred-types
                 (finalize-inference (get-function-binding main))))
           (format t "~%~&~&generate-stage: main = ~s~%" main)
           (format t "shaken = ~s~%" shaken)
           (with-output-to-string (*standard-output*)
             (format t "#version ~a~%" version)
             (loop with dumped = (make-hash-table)
                   for object in shaken
                   for stage-binding = (stage-binding object)
                   for interface-block = (when stage-binding
                                           (interface-block stage-binding))
                   unless (and interface-block (gethash interface-block dumped))
                     do (typecase object
                          ((or generic-type interface-binding constant-binding)
                           (unless (internal object)
                             (pprint-glsl object)
                             (when interface-block
                               (setf (gethash interface-block dumped) t))))
                          (global-function
                           (let ((overloads (gethash object inferred-types)))
                             (assert overloads)
                             (loop for overload in overloads
                                   for *binding-types*
                                     = (gethash overload
                                                (final-binding-type-cache
                                                 object))
                                   do (assert *binding-types*)
                                      (pprint-glsl object)))))))))))))




(in-package #:glsl)
;;; CL macros for the glsl API (for use with slime when working on files
;;;  to be loaded as glsl code)

(cl:defmacro defun (name args &body body)
  `(3bgl-shaders::compile-form '(cl:defun ,name ,args ,@body)))

(cl:defmacro defconstant (name value type)
  `(3bgl-shaders::compile-form '(%defconstant ,name ,value ,type)))

(cl:defmacro interface (name (&rest args &key in out uniform) &body slots)
  (declare (ignore in out uniform))
  `(3bgl-shaders::compile-form '(interface ,name ,args ,@slots)))

(cl:defmacro attribute (name type &rest args &key location)
  (declare (ignore location))
  `(3bgl-shaders::compile-form '(attribute ,name ,type ,@args)))

(cl:defmacro input (name type &rest args &key  stage location)
  (declare (ignore location stage))
  `(3bgl-shaders::compile-form '(input ,name ,type ,@args)))

(cl:defmacro output (name type &rest args &key stage location)
  (declare (ignore location stage))
  `(3bgl-shaders::compile-form '(output ,name ,type ,@args)))

(cl:defmacro uniform (name type &rest args &key  stage location)
  (declare (ignore location stage))
  `(3bgl-shaders::compile-form '(uniform ,name ,type ,@args)))

(cl:defmacro bind-interface (stage block-name interface-qualifier instance-name)
  `(3bgl-shaders::compile-form '(bind-interface ,stage ,block-name
                                 ,interface-qualifier ,instance-name)))

;;; glsl versions for use when whole file is processed directly
(%glsl-macro defun (name args &body body)
  `(cl:defun ,name ,args ,@body))
