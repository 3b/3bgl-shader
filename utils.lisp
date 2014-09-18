(in-package #:3bgl-shaders)

;;; newer attempt at 'nicer' interface, + helper for updating shaders

(defclass shader-program ()
  ((program :reader program :initform nil)
   ;; shader stage (as in gl:create-shader) -> name of function
   (stages :reader stages :initform (make-hash-table))
   ;; name of function -> true/false
   (dirty :reader dirty :initform (make-hash-table))
   ;; lisp name -> plist :glsl-name, :type, :dirty, :function, :value
   (uniforms :reader uniforms :initform (make-hash-table))
   ;; list of names of live uniforms for currently compiled program
   (live-uniforms :accessor live-uniforms :initform nil)
   ;; glsl name -> lisp name
   ;; (not sure if string or symbol should be primary way of
   ;; identifying a uniform, allowing both for now...)
   (name-map :reader name-map :initform (make-hash-table :test 'equal))
   ;; target glsl version
   (version :accessor version :initform 450 :initarg :version)))

(defun flag-shader (shader-program function)
  ;; only flag it if in use, otherwise hash could accumulate lots of
  ;; junk in a program with lots of shaders
  (when (and shader-program
             (nth-value 1 (gethash function (dirty shader-program))))
    (setf (gethash function (dirty shader-program)) t)))

(defparameter *stage-name-map*
  (alexandria:plist-hash-table
   '(:vertex :vertex-shader
     :fragment :fragment-shader
     :geometry :geometry-shader
     :tess-control :tess-control-shader
     :tess-evaluation :tess-evaluation-shader
     :tess-eval :tess-evaluation-shader
     :compute :compute-shader
     ;; reverse mapping
     :vertex-shader :vertex
     :fragment-shader :fragment
     :geometry-shader :geometry
     :tess-control-shader :tess-control
     :tess-evaluation-shader :tess-eval
     :tess-evaluation-shader :tess-eval
     :compute-shader :compute)))

;; if bound, is called with shader object and list of shader functions
;; in shader-function, for programs that want to track active programs
(defvar *shader-program-hook*)

(defun shader-program (&rest r
                       &key vertex fragment geometry
                         tess-control tess-evaluation
                         compute
                         &allow-other-keys)
  (declare (ignore vertex fragment geometry
                   tess-control tess-evaluation
                   compute))
  (let ((p (make-instance 'shader-program)))
    (loop for (%stage fun) on r by #'cddr
          for stage = (gethash %stage *stage-name-map* %stage)
          do (setf (gethash stage (stages p)) fun)
             (setf (gethash fun (dirty p)) t))
    (when (boundp '*shader-program-hook*)
      (funcall *shader-program-hook* p (loop for (nil a) on r by #'cddr
                                             collect a)))
    p))

;; generic interface to uniforms in program, uniform buffers, etc
(defgeneric uniform (object &rest names-and-indices))
(defmethod uniform ((program shader-program) &rest names-and-indices)
  (assert (= 1 (length names-and-indices)))
  (let ((name (car names-and-indices)))
    (when (stringp name)
      (setf name (gethash name (name-map program))))
    (getf (gethash name (uniforms program)) :value)))

;; method is (setf %uniform) so we can make a (setf uniform) that accepts
;;   (values ...)
;; simple API, for scalars, vectors, single matrix
;; new-value should be a single value, or array stuitable to pass to uniform*fv
(defmethod (setf %uniform) (new-value (program shader-program) &rest names-and-indices)
  (assert (= 1 (length names-and-indices)))
  (let ((name (car names-and-indices)))
    (when (stringp name)
      (setf name (gethash name (name-map program))))
    (setf (getf (gethash name (uniforms program)) :dirty) t)
    (when name
      (when (consp new-value)
        (setf new-value (coerce new-value 'vector)))
      (setf (getf (gethash name (uniforms program)) :value) new-value))
    new-value))

;; to be replaced with better version...
(defun (setf uniform) (new-value program &rest names-and-indices)
  (apply #'(setf %uniform) new-value program names-and-indices))

(defun %reload-program (shader-program)
  (let ((source nil)
        (shaders nil)
        (stages (alexandria:hash-table-alist (stages shader-program)))
        (program nil)
        (all-uniforms nil))
    (setf source
          (loop for (%stage . name) in stages
                for stage = (gethash %stage *stage-name-map* %stage)
                for source = nil
                do #++(format t "generating shader ~s @ ~s~%" name stage)
                   (multiple-value-bind (.source uniforms attributes)
                       (3bgl-shaders::generate-stage
                        stage name :version (version shader-program))
                     (declare (ignore attributes))
                     (setf source .source)
                     (setf all-uniforms (union all-uniforms uniforms
                                               :key 'car))
                     (loop for (l g type) in uniforms
                           do (setf (getf (gethash l (uniforms shader-program))
                                          :glsl-name)
                                    g)
                              (setf (gethash g (name-map shader-program)) l)
                              (setf
                               (getf (gethash l (uniforms shader-program))
                                     :function)
                               (ecase type
                                 (:float #'%gl:uniform-1f)
                                 (:vec2 #'gl:uniformfv)
                                 (:vec3 #'gl:uniformfv)
                                 (:vec4 #'gl:uniformfv)
                                 (:int #'%gl:uniform-1i)
                                 (:ivec2 #'gl:uniformiv)
                                 (:ivec3 #'gl:uniformiv)
                                 (:ivec4 #'gl:uniformiv)
                                 (:mat2 #'gl:uniform-matrix-2fv)
                                 (:mat2x3 #'gl:uniform-matrix-2x3-fv)
                                 (:mat2x4 #'gl:uniform-matrix-2x4-fv)
                                 (:mat3x2 #'gl:uniform-matrix-3x2-fv)
                                 (:mat3 #'gl:uniform-matrix-3fv)
                                 (:mat3x4 #'gl:uniform-matrix-3x4-fv)
                                 (:mat4x2 #'gl:uniform-matrix-4x2-fv)
                                 (:mat4x3 #'gl:uniform-matrix-4x3-fv)
                                 (:mat4 #'gl:uniform-matrix-4fv)
                                 (:sampler-1d #'gl:uniformi)
                                 (:sampler-2d #'gl:uniformi)
                                 (:sampler-3d #'gl:uniformi)))))
                collect (list %stage source)))
    ;; assuming failed compile signalled an error so won't get here
    (unwind-protect
         (progn
           (setf program (gl:create-program))
           (loop for (stage source) in source
                 for shader = (gl:create-shader stage)
                 do (push shader shaders)
                    (gl:shader-source shader source)
                    (gl:compile-shader shader)
                    (cond
                      ((gl:get-shader shader :compile-status)
                       (gl:attach-shader program shader))
                      (t
                       ;; fixme: make error printing and stream configurable
                       (format t "~s shader compile failed: ~s"
                               stage (gl:get-shader-info-log shader))
                       (return-from %reload-program nil))))
           (gl:link-program program)
           (cond
             ((gl:get-program program :link-status)
              ;; if it linked, swap with old program so we delete that on uwp
              (rotatef program (slot-value shader-program 'program)))
             (t
              ;; fixme: make error printing and stream configurable
              (format t "program link failed: ~s"
                      (gl:get-program-info-log program))
              (return-from %reload-program nil)))
           ;; update uniforms in program
           (loop for (name glsl-name) in all-uniforms
                 for index = (gl:get-uniform-location (program shader-program)
                                                      glsl-name)
                 do (setf (getf (gethash name (uniforms shader-program))
                                :index)
                          index)
                    (pushnew name (live-uniforms shader-program)))
           (maphash (lambda (k v)
                      (declare (ignore v))
                      (setf (gethash k (dirty shader-program)) nil))
                    (dirty shader-program))
           (format t "compile succeeded~%")
           ;; recompile succeeded, return T
           t)
      ;; unwind-protect cleanup: delete any created shaders, delete
      ;; any program in PROGRAM (if compile/link was successul, it
      ;; will be previous program if any)
      (loop for s in shaders
            do (gl:delete-shader s))
      (when program
        (gl:delete-program program)))))


(defmethod use-program ((program shader-program))
  (let ((dirty nil))
    (alexandria:maphash-values (lambda (k) (setf dirty (or dirty k)))
                               (dirty program))
    (when dirty
      (%reload-program program))

    (when (program program)
      (gl:use-program (program program))
      (loop for uniform-name in (live-uniforms program)
            for uniform = (gethash uniform-name (uniforms program))
            for function = (getf uniform :function)
            for value = (getf uniform :value)
            for index = (getf uniform :index)
            when (and function value index)
              do (funcall function index value)))))
