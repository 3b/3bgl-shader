(defpackage #:3bgl-shader-example
  (:use #:cl)
  (:export
   #:run-example))

(in-package #:3bgl-shader-example-shaders)
;; combinations of shaders we will be using
(cl:defparameter 3bgl-shader-example::*programs*
  '(() ;; skip an entry to use for fixed-function
    (minimal-vertex minimal-fragment)
    (normal-vertex normal-fragment)
    (lighting-vertex simple-lighting-fragment)
    (lighting-vertex shiny-fragment)
    (lighting-vertex noisy-shiny-fragment)))

(in-package #:3bgl-shader-example)



(defclass 3bgl-shader-example-window (glut:window)
  ((programs :accessor programs :initform nil)
   (index :accessor index :initform 0))
  (:default-initargs :width 640 :height 480 :title "3bgl-shader"
                     :mode '(:double :rgb :depth :multisample)))


(defparameter *teapot-rotation-x* 0.0)
(defparameter *teapot-rotation-y* 0.0)
(defparameter *teapot-rotation-z* 0.0)

(defun draw-teapot ()
  (gl:clear-color 0 0.3 0.5 1.0)
  (gl:clear :color-buffer :depth-buffer)

  (gl:disable :blend :texture-2d)
  (gl:enable :lighting :light0 :depth-test)

  (gl:color-material :front :ambient-and-diffuse)

  (gl:light :light0 :position '(0 1 1 0))
  (gl:light :light0 :diffuse '(0.2 0.4 0.6 0))

  (gl:color 1 1 1)
  (glut:solid-teapot 1.3)

  (incf *teapot-rotation-x* 0.01)
  (incf *teapot-rotation-y* 0.05)
  (incf *teapot-rotation-z* 0.03))




(defparameter *modified-shader-functions* nil)

(defun modified-shader-hook (modified)
  (format t "saw modified functions ~s~%" modified)
  (setf *modified-shader-functions*
        (union modified *modified-shader-functions*)))

(pushnew 'modified-shader-hook 3bgl-shaders::*modified-function-hook*)

(defun recompile-modified-shaders (w)
  (let* ((m *modified-shader-functions*)
         (current-program (nth (index w) (programs w))))
    ;; flag any shaders we are using
    (loop for (p f names) in (programs w)
          do (loop for n in names
                   for i from 0
                   when (member n m)
                     do (setf (nth i f) t)))
    ;; fixme: this needs a lock, since it could be modified from
    ;; another thread
      (setf *modified-shader-functions* nil)

    (destructuring-bind (&optional program flags names)
        current-program
      (when (and names
                 (or (and program (some #'identity flags))
                     (and (not program) (every #'identity flags))))
        (format t "~%recompiling shader program ~s for changes in functions:~&  ~a~%"
                (index w)
                (loop for f in flags for n in names when f collect n))
        (setf (second current-program) (substitute nil t flags))
        (time
         (setf (car current-program)
               (3bgl-shaders::reload-program (first current-program)
                                             (first (third current-program))
                                             (second (third current-program))
                                             :geometry (third (third current-program))
                                             :version 330)))))))


(defparameter *w* nil)
(defmethod glut:display ((w 3bgl-shader-example-window))
  (with-simple-restart (continue "continue")
    (setf *w* w)
    (recompile-modified-shaders w)

    (gl:with-pushed-matrix* (:projection)
      (gl:load-identity)
      (glu:perspective 45 (/ (glut:width w)
                             (glut:height w))
                       0.5 20)
      (gl:with-pushed-matrix* (:modelview)
        (gl:load-identity)

        (flet ((radians (x) (coerce (/ (* pi x) 180) 'single-float))
               (v (x y z) (sb-cga:vec (float x 1.0)
                                      (float y 1.0)
                                      (float z 1.0))))
          (let* ((m (sb-cga:matrix*
                     ;; some versions of glut transform the teapot
                     ;; with gl matrix, so duplicate that here
                     ;; (might not look right on newest freeglut though)
                     (sb-cga:rotate-around (v 1 0 0)
                                           (radians 270))
                     (sb-cga:scale* (* 1.3 0.5) (* 1.3 0.5) (* 1.3 0.5))
                     (sb-cga:translate (v 0 0 -1.5))))
                 (v (sb-cga:matrix*
                      (sb-cga:translate (v 0 0 -4))
                      (sb-cga:rotate-around (v 1 0 0)
                                            (radians *teapot-rotation-x*))
                      (sb-cga:rotate-around (v 0 1 0)
                                            (radians *teapot-rotation-y*))
                      (sb-cga:rotate-around (v 0 0 1)
                                            (radians *teapot-rotation-z*))))
                 (p (kit.math:perspective-matrix 45
                                                 (/ (glut:width w)
                                                    (glut:height w))
                                                 0.5 20))
                 (mv (sb-cga:matrix* v m))
                 (mvp (sb-cga:matrix* p v m))
                 (p1 (car (nth (index w) (programs w)))))
            (gl:load-matrix v)
            (gl:matrix-mode :projection)
            (gl:load-matrix p)
            (gl:matrix-mode :modelview)
            (when p1
              (gl:use-program p1)
              (3bgl-shaders::uniformf p1 "time"
                                      (float
                                       (/ (get-internal-real-time)
                                          internal-time-units-per-second)))
              (3bgl-shaders::uniform-matrix p1 "mv" mv)
              (3bgl-shaders::uniform-matrix p1 "mvp" mvp)
              (3bgl-shaders::uniform-matrix p1 "normalMatrix" mv))))

        (draw-teapot)
        (gl:use-program 0)))

    (glut:swap-buffers)))

(defmethod glut:idle ((w 3bgl-shader-example-window))
  (glut:post-redisplay))

(defmethod glut:reshape ((w 3bgl-shader-example-window) width height)
  (setf (glut:width w) width
        (glut:height w) height)
  (gl:viewport 0 0 (glut:width w) (glut:height w)))

(defmethod glut:keyboard ((w 3bgl-shader-example-window) key x y)
  (declare (ignore x y))
  (with-simple-restart
      (continue "continue")
    (case key
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
      (let* ((n (digit-char-p key))
             (names (nth n *programs*)))
        (setf (index w) n)
        (when names
          (unless (nth n (programs w))
            (unless (> (length (programs w)) n)
              (setf (programs w) (replace (make-list (1+ n)
                                                     :initial-element nil)
                                          (programs w))))
            (setf (nth n (programs w))
                  ;; for each program we store program object, list of
                  ;; modified flags, and list of shader functions
                  ;; (vertex, fragment, optional geometry)
                  (list nil
                        (make-list (length names) :initial-element t)
                        names))
            (format t "added program ~s: ~s~%" n (programs w)))
          )
        )
      )
     (#\Esc
      (glut:destroy-current-window)))))


(defun run-example ()
  (glut:display-window (make-instance '3bgl-shader-example-window)))
