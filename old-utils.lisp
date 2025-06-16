(in-package #:3bgl-shaders)
;;; some brain-dead utils for shader-related stuff (recompiling
;;; shaders, setting uniforms by name, etc)

;; print shaders as compiled for debugging
(defparameter *print-shaders* nil)

(defun uniform-index (program name)
  (if program
      (gl:get-uniform-location program name)
      -1))

(defun uniformi (program name value)
  (gl:uniformi (uniform-index program name) value))

(defun uniformf (program name x &optional y z w)
  (let ((u (uniform-index program name)))
    (unless (minusp u)
      (cond
      (w (%gl:uniform-4f u (float x) (float y) (float z) (float w)))
      (z (%gl:uniform-3f u (float x) (float y) (float z)))
      (y (%gl:uniform-2f u (float x) (float y)))
      (x (%gl:uniform-1f u (float x)))))))

(defun uniformfv (program name v)
  (let ((u (uniform-index program name)))
    (unless (minusp u)
      (typecase v
        ;; fast cases
        ((vector single-float 3)
         (%gl:uniform-3f u (aref v 0) (aref v 1) (aref v 2)))
        ((vector single-float 4)
         (%gl:uniform-4f u (aref v 0) (aref v 1) (aref v 2) (aref v 3)))
        ;; convenient but slower cases
        ((vector * 4)
         (%gl:uniform-4f u (float (elt v 0) 1.0) (float (elt v 1) 1.0)
                         (float (elt v 2) 1.0) (float (elt v 3) 1.0)))
        ((vector * 3)
         (%gl:uniform-3f u (float (elt v 0) 1.0) (float (elt v 1) 1.0)
                         (float (elt v 2) 1.0)))
        ((vector * 2)
         (%gl:uniform-2f u (float (elt v 0) 1.0) (float (elt v 1) 1.0)))

        ((vector * 1)
         (%gl:uniform-1f u (float (elt v 0) 1.0)))

        ))))

(defun uniform-matrix (program name m)
  (let ((u (uniform-index program name)))
    (unless (minusp u)
      (gl:uniform-matrix u 4 (vector m) nil))))



(defun reload-program (old v f &key errorp (verbose t) geometry (version 450))
  "compile program from shaders named by V and F, on success, delete
program OLD and return new program, otherwise return OLD"
  ;; intended to be used like
  ;;  (setf (program foo) (reload-program (program foo) 'vertex 'frag))
  (let ((vs (gl:create-shader :vertex-shader))
        (fs (gl:create-shader :fragment-shader))
        (gs (when geometry (gl:create-shader :geometry-shader)))
        (program (gl:create-program))
        (uniformh (make-hash-table)))
    (unwind-protect
         (flet ((c (stage entry)
                  (multiple-value-bind (source uniforms attributes buffers
                                        structs)
                      (3bgl-shaders::generate-stage stage entry
                                                    :version version)
                    (declare (ignorable attributes buffers structs))
                    (loop for u in uniforms
                          for (l g tt) = u
                          for o = (gethash l uniformh)
                          when (and o (not (equalp u o)))
                            do (format t "duplicate uniform ~s -> ~s~%?" o u)
                          do (setf (gethash l uniformh)
                                   (cons -1 u)))
                    source))
                (try-shader (shader source)
                  (when *print-shaders*
                    (format t "generating shader ~s~%" shader)
                     (format t "~s~%" source))
                  (gl:shader-source shader source)
                  (gl:compile-shader shader)
                  (cond
                    ((gl:get-shader shader :compile-status)
                     (gl:attach-shader program shader))
                    (errorp
                     (error "shader compile failed: ~s" (gl:get-shader-info-log shader)))
                    (t
                     (when verbose
                       (format verbose "shader compile failed: ~s" (gl:get-shader-info-log shader)))
                     (return-from reload-program old)))))
           (try-shader vs (c :vertex v))
           (try-shader fs (c :fragment f))
           (when gs
             (try-shader gs (c :geometry geometry)))
           (gl:link-program program)
           (cond
             ((gl:get-program program :link-status)
              ;; if it linked, swap with old program so we delete that on uwp
              (rotatef old program))
             (errorp
              (error "program link failed ~s"
                     (gl:get-program-info-log program)))
             (t
              (when verbose
                (format verbose "program link failed: ~s" (gl:get-program-info-log program))))))
      ;; clean up on exit
      (gl:delete-shader vs)
      (gl:delete-shader fs)
      ;; PROGRAM is either program we just tried to link, or previous one if
      ;; link succeeded
      (when program
        (gl:delete-program program)))
    (when old
      (loop for u being the hash-keys of uniformh
            for n = (third (gethash u uniformh))
            do (setf (car (gethash u uniformh))
                     (uniform-index old n))))
    (values old uniformh)))

(defparameter *normalize-shader-types*
  (alexandria:plist-hash-table
   '(:vertex :vertex-shader
     :fragment :fragment-shader
     :geometry :geometry-shader
     :tess-control :tess-control-shader
     :tess-eval :tess-evaluation-shader
     :compute :compute-shader)))

(defun reload-program* (old stages &key errorp (verbose t) (version 450)
                                     (print *print-shaders*))
  "compile program from STAGES, a plist of stage names and entry
points. on success, delete program OLD and return new program,
otherwise return OLD"
  ;; intended to be used like
  ;;  (setf (program foo) (reload-program (program foo) '(:vertex v :fragment f))
  (let ((shaders ())
        (program (gl:create-program))
        (uniformh (make-hash-table)))
    (unwind-protect
         (labels ((c (stage entry)
                    (multiple-value-bind (source uniforms attributes buffers
                                          structs)
                        (3bgl-shaders::generate-stage stage entry
                                                      :version version)
                      (declare (ignorable attributes buffers structs))
                      (loop for u in uniforms
                            for (l g tt) = u
                            for o = (gethash l uniformh)
                            when (and o (not (equalp u o)))
                              do (format t "duplicate uniform ~s -> ~s~%?" o u)
                            do (setf (gethash l uniformh)
                                     (cons -1 u)))
                      source))
                  (try-shader (stage entry-point)
                    (let ((source (c stage entry-point))
                          (shader (gl:create-shader stage)))
                      (push shader shaders)
                      (when print
                        (format t "generating shader ~s~%" shader)
                        (format t "~s~%" source))
                      (gl:shader-source shader source)
                      (gl:compile-shader shader)
                      (cond
                        ((gl:get-shader shader :compile-status)
                         (gl:attach-shader program shader))
                        (errorp
                         (error "shader compile failed: ~s" (gl:get-shader-info-log shader)))
                        (t
                         (when verbose
                           (format verbose "shader compile failed: ~s" (gl:get-shader-info-log shader)))
                         (return-from reload-program* old))))))
           (loop for (.stage entry) on stages by #'cddr
                 for stage = (gethash .stage *normalize-shader-types* .stage)
                 do (try-shader stage entry))
           (gl:link-program program)
           (cond
             ((gl:get-program program :link-status)
              ;; if it linked, swap with old program so we delete that on uwp
              (rotatef old program))
             (errorp
              (error "program link failed ~s"
                     (gl:get-program-info-log program)))
             (t
              (when verbose
                (format verbose "program link failed: ~s" (gl:get-program-info-log program))))))
      ;; clean up on exit
      (map 'nil 'gl:delete-shader shaders)
      ;; PROGRAM is either program we just tried to link, or previous one if
      ;; link succeeded
      (when program
        (gl:delete-program program)))
    (when old
      (loop for u being the hash-keys of uniformh
            for n = (third (gethash u uniformh))
            do (setf (car (gethash u uniformh))
                     (uniform-index old n))))
    (values old uniformh)))

