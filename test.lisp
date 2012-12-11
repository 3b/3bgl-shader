(defpackage #:3bgl-shader-test
  (:use #:cl #:basecode #:glsl)
  (:shadowing-import-from #:cl #:defun #:defconstant))

(in-package #:3bgl-shader-test)

(defclass noise-demo (basecode::basecode-glop
                      basecode:perspective-projection
                      basecode:basecode-clear
                      basecode:fps-graph
                      basecode:basecode-draw-axes
                      basecode:basecode-draw-ground-plane
                      basecode:freelook-camera
                      basecode::basecode-exit-on-esc)
  ((program :initform nil :accessor program))
  (:default-initargs :look-at-eye '(8 10 4)))

(glsl-defun mod289 (x)
  (declare (:vec4 x) (values :vec4))
  (return (- x (* 289.0 (floor (* x (/ 289.0)))))))

(glsl-defun mod289f (x)
  (declare (:float x) (values :float))
  (return (- x (* 289.0 (floor (* x (/ 289.0)))))))

(glsl-defun permute (x)
  (declare (:vec4 x) (values :vec4))
  (return (mod289 (* x (+ 1.0 (* x 34.0))))))

(glsl-defun permutef (x)
  (declare (:float x) (values :float))
  (return (mod289f (* x (+ 1.0 (* x 34.0))))))

(glsl-defun taylor-inv-sqrt (r)
  (declare (:vec4 r) (values :vec4))
  (return (- 1.79284291400159 (0.85373472095314 * r))))

(glsl-defun taylor-inv-sqrtf (r)
  (declare (:float r) (values :float))
  (return (- 1.79284291400159 (0.85373472095314 * r))))

(glsl-defun grad4 (j ip)
  (declare (:float j) (:vec4 ip) (values :vec4))
  (let ((ones (vec4 1.0 1.0 1.0 -1.0))
        (p) (s))
    (declare (:vec4 ones p s))
    (setf (@ p xyz) (- (* (floor (* (fract (* (vec3 j) (@ ip xyz)))
                                    7.0))
                           (@ ip z))
                       1.0)
          (@ p w) (- 1.5 (dot (abs (@ p xyz)) (@ ones xyz)))
          s (vec4 (less-than p (vec4 0.0)))
          (@ p xyz) (+ (@ p xyz) (* (- (* (@ s xyz) 2.0) 1.0)
                                    (@ s www))))
    (return p)))

(glsl-defconstant +f4+ 0.309016994374947451 :float)

(glsl-defun snoise (v)
  (declare (values :float) (:vec4 v))
  (let* ((c (vec4 0.138196601125011 ; (5 - sqrt(5))/20 G4
                  0.276393202250021 ; 2 * G4
                  0.414589803375032 ; 3 * G4
                  -0.447213595499958)) ; -1 + 4 * G4
         ;; first corner
         (i (floor (+ v (dot v (vec4 +f4+)))))
         (x0 (+ (- v i) (dot i (@ c xxxx))))
         ;; other corners
         (i0)
         (is-x (step (@ x0 yzw) (@ x0 xxx)))
         (is-yz (step (@ x0 zww) (@ x0 yyz))))
    (declare (:vec4 c i x0 i0)
             (:vec3 is-x is-yz))
    (setf (@ i0 x) (+ (@ is-x x) (@ is-x y) (@ is-x z))
          (@ i0 yzw) (- 1.0 is-x)
          (@ i0 y) (+ (@ i0 y) (@ is-yz x) (@ is-yz y))
          (@ i0 zw) (+ (@ i0 zw) (- 1.0 (@ is-yz xy)))
          (@ i0 z) (+ (@ i0 z) (@ is-yz z))
          (@ i0 w) (+ (@ i0 w) (- 1.0 (@ is-yz z))))
    ;; i0 now contains the unique values 0,1,2,3 in each channel
    (let* ((i3 (clamp i0 0.0 1.0))
           (i2 (clamp (- i0 1.0) 0.0 1.0))
           (i1 (clamp (- i0 2.0) 0.0 1.0))
           (x1 (+ (- x0 i1) (@ C xxxx)))
           (x2 (+ (- x0 i2) (@ C yyyy)))
           (x3 (+ (- x0 i3) (@ C zzzz)))
           (x4 (+ x0 (@ C wwww)))
           (i (mod289 i))
           (j0 (permutef (+ (permutef (+ (permutef (+ (permutef (@ i w))
                                                      (@ i z)))
                                       (@ i y)))
                           (@ i x))))
           (j1 (macrolet ((p-i* (p z)
                            `(permute (+ ,@(when p (list p))
                                         (@ i ,z)
                                         (vec4 (@ i1 ,z) (@ i2 ,z)
                                               (@ i3 ,z) 1.0)))))
                 (p-i* (p-i* (p-i* (p-i* nil w) z) y) x)))
           (ip (vec4 (/ 294.0) (/ 49.0) (/ 7.0) 0.0))
           (p0 (grad4 j0 ip))
           (p1 (grad4 (@ j1 x) ip))
           (p2 (grad4 (@ j1 y) ip))
           (p3 (grad4 (@ j1 z) ip))
           (p4 (grad4 (@ j1 w) ip))
           (norm (taylor-inv-sqrt (vec4 (dot p0 p0)
                                        (dot p1 p1)
                                        (dot p2 p2)
                                        (dot p3 p3)))))
      (declare (:vec4 i i1 i2 i3 x1 x2 x3 x4 j1 ip p0 p1 p2 p3 p4 norm)
               (:float j0))
      (setf p0 (* p0 (@ norm x))
            p1 (* p1 (@ norm y))
            p2 (* p2 (@ norm z))
            p3 (* p3 (@ norm w))
            p4 (* p4 (taylor-inv-sqrtf (dot p4 p4))))
      (let* ((m0 (max (- 0.6 (vec3 (dot x0 x0) (dot x1 x1) (dot x2 x2)))
                      0.0))
             (m1 (max (- 0.6 (vec2 (dot x3 x3) (dot x4 x4)))
                      0.0)))
        (declare (:vec3 m0) (:vec2 m1))
        (setf m0 (* m0 m0)
              m1 (* m1 m1))
        (return (* 49.0 (+ (dot (* m0 m0) (vec3 (dot p0 x0)
                                                (dot p1 x1)
                                                (dot p2 x2)))
                           (dot (* m1 m1) (vec2 (dot p3 x3)
                                                (dot p4 x4))))))))))

(glsl-interface varyings (:out (:vertex outs) :in (:fragment t))
  (light-dir :vec3)
  (eye-dir :vec3)
  (uv :vec2))

(glsl-interface light (:uniform light)
  (color :vec4)
  (position :vec4))

#++
(glsl-interface misc (:uniform (:fragment misc))
  (time :float))

#++
(glsl-interface transforms (:uniform t)
  (mvp :mat4)
  (normal-matrix :mat4))
(glsl-uniform mvp :mat4)
(glsl-uniform time :float)

(glsl-input position :vec4 :stage :vertex)
(glsl-input uv :vec2 :stage :vertex)

(glsl-defun vertex ()
  (declare (values))
  (setf gl-position (* mvp position))
  (setf (@ outs uv) uv))

(glsl-output out-color :vec4 :stage :fragment)

(glsl-defun frag ()
  (declare (values))
  (let ((uv (- uv (/ time 22))))
    (declare (:vec2 uv))
    (setf out-color
                #++(vec4 0.5 (@ uv x) (@ uv y) 1.0)
                (vec4 (+ 0.0
                         (* 1 (snoise (vec4 (* 3 uv) (/ time 10) (/ time 5))))
                         (* 0.5 (snoise (vec4 (* 4.3 uv) 0.0 (/ time 4.6))))
                         (* 0.3 (snoise (vec4 (* 5.1 uv) 0.0 (/ time 3.9))))
                         (* 0.2 (snoise (vec4 (* 8.1 uv) 0.0 (/ time 3.9))))
                         (* 0.1 (snoise (vec4 (* 15.1 uv) 0.0 (/ time 1.9))))
                         (* 0.05 (snoise (vec4 (* 25.1 uv) 0.0 (/ time 1.9))))
                         (* 0.025 (snoise (vec4 (* 45.1 uv) 0.0 (/ time 1.0))))
                         )
                      0.0  ;(snoise (vec4 (* 3 uv) 0.0 (/ time 2.34)))
                      0.0  ;(snoise (vec4 (* 2 uv) 0.0 (/ time 5.43)))
                      1.0))))

(defun reload-program (w)
  (let ((vs (gl:create-shader :vertex-shader))
        (fs (gl:create-shader :fragment-shader))
        (program (gl:create-program))
        (error nil)
        (verbose t))
    (unwind-protect
         (flet ((try-shader (shader source)
                  (format t "compiling shader:~% ~s~%" source)
                  (gl:shader-source shader source)
                  (gl:compile-shader shader)
                  (cond
                    ((gl:get-shader shader :compile-status)
                     (gl:attach-shader program shader))
                    (error
                     (error "shader compile failed: ~s" (gl:get-shader-info-log shader)))
                    (t
                     (format (or verbose t) "vertex shader compile failed: ~s" (gl:get-shader-info-log shader))))))
           (try-shader vs (generate-stage :vertex 'vertex))
           (try-shader fs (generate-stage :fragment 'frag))
           (gl:link-program program)
           (cond
             ((gl:get-program program :link-status)
              ;; if it linked, swap with old program so we delete that on uwp
              (rotatef (program w) program))
             (error
              (error "program link failed ~s"
                     (gl:get-program-info-log program)))
             (t
              (format (or verbose t) "program link failed: ~s" (gl:get-program-info-log program)))))
      ;; clean up on exit
      (gl:delete-shader vs)
      (gl:delete-shader fs)
      ;; PROGRAM is either program we just tried to link, or previous one if
      ;; link succeeded
      (when program
        (gl:delete-program program)))))

(defun uniform-index (w name)
  (if (program w)
      (gl:get-uniform-location (program w) name)
      -1))

(defun uniformi (w name value)
  (gl:uniformi (uniform-index w name) value))

(defun uniformf (win name x &optional y z w)
  (let ((u (uniform-index win name)))
    (unless (minusp u)
      (cond
      (w (%gl:uniform-4f u (float x) (float y) (float z) (float w)))
      (z (%gl:uniform-3f u (float x) (float y) (float z)))
      (y (%gl:uniform-2f u (float x) (float y)))
      (x (%gl:uniform-1f u (float x)))))))

(defmethod basecode:basecode-draw ((w noise-demo))
  (declare (optimize (debug 2)))
  (gl:enable :depth-test)
  (let ((uv 1))
    (gl:with-pushed-matrix* (:modelview)
      (gl:disable :cull-face :lighting)
      (gl:color 1 1 1 1)
      (gl:scale 4 4 4)
      (when (program w)
        (gl:use-program (program w))
        (let* ((p (gl:get* :projection-matrix))
               (mv (gl:get* :modelview-matrix))
               (mvp (sb-cga:matrix* (sb-cga:transpose-matrix mv)
                                    (sb-cga:transpose-matrix p))))
          (gl::with-opengl-sequence (m '%gl:float
                                         mvp)
            (%gl:uniform-matrix-4fv (uniform-index w "mvp")
                                    1 t
                                    m)
            (uniformf w "time" (/ (get-internal-real-time)
                                  (float internal-time-units-per-second 1.0)))))
        (setf uv (gl:get-attrib-location (program w) "uv")))
      (when (< uv 0) (setf uv 1))
      (gl:point-size 5)
      (gl:with-primitives :quads
        (gl:tex-coord 0 0)
        (gl:vertex-attrib uv 0 0)
        (gl:vertex -1 0.1 -1)

        (gl:tex-coord 0 2)
        (gl:vertex-attrib uv 0 2)
        (gl:vertex -1 0.1 1)

        (gl:tex-coord 2 2)
        (gl:vertex-attrib uv 2 2)
        (gl:vertex 1 0.1 1)

        (gl:tex-coord 2 0)
        (gl:vertex-attrib uv 2 0)
        (gl:vertex 1 0.1 -1))))
  (gl:use-program 0))

(defmethod key-down ((w noise-demo) k)
  (declare (optimize (debug 2)))
  (case k
    (:r (with-simple-restart (continue "ignore")
          (reload-program w)))))



; (basecode:basecode-run (make-instance 'noise-demo))
