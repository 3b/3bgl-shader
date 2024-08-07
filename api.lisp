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
file using the 3bgl-glsl:defun macro is recompiled, so probably should
store names and only update shader programs at next frame rather
than updating programs directly from hook function.")

(defvar *default-version* 450)
(defvar *default-extensions* nil) ;; exact strings or like :arb-some-extension

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
      (3bgl-glsl::with-package-environment ()
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
                  for deps = (bindings-using i)
                  do (loop for i being the hash-keys of deps
                           when (typep i 'global-function)
                             do (setf (gethash i modified-deps) t)))
            (loop for (nil i) in *new-global-definitions*
                  for deps = (bindings-using i)
                  do (loop for i being the hash-keys of deps
                           when (typep i 'global-function)
                             do (setf (gethash i modified-deps) t)))
            (loop for i in *new-function-definitions*
                  do (setf (gethash i modified-deps) t))
            (when *verbose*
              (format t "deps = ~s~%" (mapcar 'name (alexandria:hash-table-keys modified-deps))))
            (when (plusp (hash-table-count modified-deps))
              (let ((modified (infer-modified-functions
                               (alexandria:hash-table-keys modified-deps))))
                (assert modified)
                (loop for f in modified
                      do (pushnew (name f) modified-function-names)))))

          (when *verbose*
            (format t "modified functions: ~s~%" modified-function-names)
            (format t "modified types: ~s~%" *new-type-definitions*)
            (format t "modified globals: ~s~%" *new-global-definitions*)))))
    ;; call hook outside lock in case it tries to call generate-stage
    (map nil (lambda (a) (funcall a modified-function-names))
         *modified-function-hook*)
    nil))

(defun expand-extension-keyword (ext)
  (if (stringp ext)
      ext
      (let* ((s (substitute #\_ #\- (symbol-name ext)))
             (p (position #\_ s)))
        (format nil "GL_~:@(~a~)~(~a~)" (subseq s 0 p) (subseq s p)))))

(defmethod generate-output (objects inferred-types (backend (eql :glsl))
                            &key version extensions &allow-other-keys)
  (with-output-to-string (*standard-output*)
    (format t "#version ~a~%" version)
    (loop for .ext in extensions
          for ext = (if (consp .ext) (first .ext) .ext)
          for enable = (if (consp .ext) (second .ext) t)
          do (format t "#extension ~a : ~a~%"
                     (expand-extension-keyword ext)
                     (if enable "enable" "disable")))
    (when (eql *current-shader-stage* :vertex)
      (format t "invariant gl_Position;~%"))
    ;; put layout() at beginning for compute stage
    (when (and (eql *current-shader-stage* :compute)
               (layout-qualifiers *print-as-main*))
      (print-main-layout-qualifiers (layout-qualifiers *print-as-main*)))

    (loop with dumped = (make-hash-table)
          for object in objects
          for stage-binding = (stage-binding object)
          for interface-block = (when stage-binding
                                  (interface-block stage-binding))
          unless (and interface-block (gethash interface-block dumped))
            do (etypecase object
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
                             (pprint-glsl object))))))))

(defparameter *default-backend* :glsl)

(defparameter *shader-type->stage*
  (alexandria:plist-hash-table
   '(:vertex-shader :vertex
     :fragment-shader :fragment
     :geometry-shader :geometry
     :tess-control-shader :tess-control)))

(defmethod expand-uniform-slots (prefix (b binding))
  (append (expand-uniform-slots prefix (value-type b))))

(defmethod expand-uniform-slots (prefix (type struct-type))
     (loop for slot in (bindings type)
           for slot-name = (name slot)
           for slot-type = (value-type slot)
           append (expand-uniform-slots (cons slot-name prefix) slot-type)))

(defmethod expand-uniform-slots (prefix (type concrete-type))
  (list (reverse prefix)))

(defmethod expand-uniform-slots (prefix (type array-type))
  (let ((size (array-size type)))
    (etypecase size
      (number
       (loop for i below size
             append (expand-uniform-slots (cons i prefix) (base-type type))))
      (constant-binding
       (unless (numberp (initial-value-form size))
         (error "can't expand constant ~s = ~s when generating uniforms"
                (name size) (initial-value-form size)))
       (loop for i below (initial-value-form size)
             append (expand-uniform-slots (cons i prefix) (base-type type))))
      ((eql :*)
       (cons '[] prefix)))))


(defun expand-uniforms (uniforms expand)
  (loop for u in uniforms
        for sb = (stage-binding u)
        for b = (binding sb)
        collect (list* (name u) (translate-name u)
                       (name (if (typep b 'binding)
                                 (value-type b)
                                 b))
                       (when expand
                         (list :components
                               (expand-uniform-slots (list (name u))
                                                     (binding sb)))))))

(defun expand-buffers (buffers)
  (let ((blocks (delete-duplicates
                 (loop for b in buffers
                       collect (stage-binding b))
                 :test 'equalp
                 :key 'interface-block)))
    (loop for block in blocks
          for ib = (interface-block block)
          for lq = (layout-qualifier block)
          collect (list* (name ib)
                         (translate-name ib)
                         (list :layout lq
                               :components
                               (when ib
                                (loop for b in (bindings ib)
                                      collect (list (name b)
                                                    (name (if (typep b 'binding)
                                                              (value-type b)
                                                              b))))))))))

(defun expand-structs (structs)
  (loop for struct in structs
        collect (list* (name struct)
                       (translate-name struct)
                       (list :components
                             (loop for b in (bindings struct)
                                   collect (list (name b)
                                                 (name (if (typep b 'binding)
                                                           (value-type b)
                                                           b))))))))

;; final pass of compilation
;; finish type inference for concrete types, generate glsl
(defun generate-stage (stage main &key (backend *default-backend*)
                                    (version *default-version*)
                                    (extensions *default-extensions*)
                                    (expand-uniforms))
  "Generate GLSL shader for specified STAGE, using function named by
MAIN as glsl 'main' function. ROOT and all functions/variables/etc it
depends on should already have been successfully compiled with
COMPILE-FORM. STAGE is :VERTEX, :FRAGMENT, :GEOMETRY, :TESS-EVAL,
:TESS-CONTROL, or :COMPUTE. VERSION specifies the value of the version
pragma in generated shader, but doesn't otherwise affect generated
code currently. Returns a list of active uniforms in the
form (LISP-NAME \"glslName\" type . PROPERTIES) as second value, and a
list of active attributes in same format as third value. (GL shader
types like :VERTEX-SHADER are also accepted for STAGE)

For uniforms, PROPERTIES is a plist containing 0 or more of:

:COMPONENTS : (when EXPAND-UNIFORMS is true) for composite
uniforms (structs, etc), a list containing a list of uniform name and
slot names or array indices for each leaf uniform represented by the
type, for example a struct uniform containing an array of structs
might have entries that look like (foo bar 1 baz) corresponding to the
uniform \"foo.bar[1].baz\".


"
  ;; todo: add location, layout, binding, UBO/SSBO, etc for uniforms
  (setf stage (gethash stage *shader-type->stage* stage))
  (bordeaux-threads:with-lock-held (*compiler-lock*)
    (3bgl-glsl::with-package-environment (main)
      (let* ((*print-as-main* (get-function-binding main))
             (*current-shader-stage* stage)
             (uniforms)
             (buffers)
             (attributes)
             (structs))
        (let ((shaken (tree-shaker main)))
          (let ((inferred-types
                  (finalize-inference (get-function-binding main))))
            #++(format t "~%~&~&generate-stage: main = ~s~%" main)
            #++(format t "shaken = ~s~%" shaken)
            (loop for s in shaken
                  for i = (when (typep s 'interface-binding)
                            (stage-binding s))
                  when (typep s 'struct-type)
                    do (push s structs)
                  when i
                    do #++(format t " ~s binding ~s / ~s = ~s~%"
                               (interface-qualifier i)
                                  (name s) (translate-name s)
                                  (name (binding i)))
                       (case (if (consp (interface-qualifier i))
                              (car (interface-qualifier i))
                              (interface-qualifier i))
                         (:uniform
                          #++(pushnew (list (name s) (translate-name s)
                                         (name (binding i)))
                                   uniforms :test 'equal)
                          (pushnew s uniforms :test 'equal))
                         (:buffer
                          (pushnew s buffers :test 'equal))
                         (:in
                          (when (eq stage :vertex)
                            (pushnew (list (name s) (translate-name s)
                                           (name (binding i)))
                                     attributes :test 'equal)))))
                                        ;(break "shaken" shaken)
            (values
             (generate-output shaken inferred-types backend
                              :version version :extensions extensions)
             (expand-uniforms uniforms expand-uniforms)
             attributes
             (expand-buffers buffers)
             (expand-structs structs))))))))




(in-package #:3bgl-glsl)
;;; CL macros for the glsl API (for use with slime when working on files
;;;  to be loaded as glsl code)

(cl:defmacro defun (name args &body body)
  `(3bgl-shaders::compile-form '(cl:defun ,name ,args ,@body)))

(cl:defmacro defmacro (name args &body body)
  `(3bgl-shaders::compile-form '(cl:defmacro ,name ,args ,@body)))

(cl:defmacro defconstant (name value type)
  `(3bgl-shaders::compile-form '(%defconstant ,name ,value ,type)))

(cl:defmacro defstruct (name &rest slots)
  `(3bgl-shaders::compile-form '(cl:defstruct ,name ,@slots)))

(cl:defmacro interface (name (&rest args &key in out uniform buffer
                                           layout)
                        &body slots)
  (declare (ignore in out uniform buffer layout))
  `(3bgl-shaders::compile-form '(interface ,name ,args ,@slots)))

(cl:defmacro attribute (name type &rest args &key location)
  (declare (ignore location))
  `(3bgl-shaders::compile-form '(attribute ,name ,type ,@args)))

(cl:defmacro input (name type &rest args &key  stage location qualifiers)
  (declare (ignore location stage))
  `(3bgl-shaders::compile-form '(input ,name ,type ,@args)))

(cl:defmacro output (name type &rest args &key stage location qualifiers)
  (declare (ignore location stage))
  `(3bgl-shaders::compile-form '(output ,name ,type ,@args)))

(cl:defmacro uniform (name type &rest args &key  stage location internal layout
                                             qualifiers default
                      &allow-other-keys)
  (declare (ignore location stage internal layout qualifiers default))
  `(3bgl-shaders::compile-form '(uniform ,name ,type ,@args)))

(cl:defmacro shared (name type &rest args &key  stage layout qualifiers
                     &allow-other-keys)
  (declare (ignore stage layout qualifiers))
  `(3bgl-shaders::compile-form '(shared ,name ,type ,@args)))

(cl:defmacro bind-interface (stage block-name interface-qualifier instance-name)
  `(3bgl-shaders::compile-form '(bind-interface ,stage ,block-name
                                 ,interface-qualifier ,instance-name)))

;;; glsl versions for use when whole file is processed directly
(%glsl-macro defun (name args &body body)
  `(cl:defun ,name ,args ,@body))



;; define cl:position as a vec4 vertex attribute at location 0, since
;; it is pretty common but can't be defined from user code with CL
;; package locked
(3bgl-glsl::glsl-input position :vec4 :location 0)
;; and single-float pi as well
(3bgl-glsl::glsl-defconstant pi #.(float pi 1.0) :float)

;; lock CL abd GLSL packages, so user packages can't define
;; conflicting globals with names in those packages
(setf (3bgl-shaders::locked 3bgl-shaders::*cl-environment*) t
      (3bgl-shaders::locked 3bgl-glsl::*glsl-base-environment*) t)
