(in-package #:3bgl-shaders)

(defvar *spirv-output*)
(defvar *used-globals*)
(defvar *current-arg-type* nil)
(defmacro with-spirv-output (() &body body)
  `(let ((*spirv-output* nil)
         (*used-globals* nil))
     ,@body))

(defun add-spirv (form)
  (push (print form) *spirv-output*))
(defun get-spirv ()
  (nreverse *spirv-output*))

(defun add-global (form)
  (pushnew form *used-globals*))
(defun get-globals ()
  (nreverse *used-globals*))

(defgeneric dump-spirv (object))

(defmethod dump-spirv (object)
  (cerror "continue" "dumping unknown object ~s (~s)?" object
          (list *current-call* *binding-types*
                (gethash *current-call* *binding-types*)))
  (format t "dumping unknown object ~s?~%" object)
  nil)

(defmethod dump-spirv ((o interface-binding))
  (let ((b (stage-binding o))
        (interface)
        (decorations))
    (loop with l = (alexandria:ensure-list (interface-qualifier b))
          for i in '(:in :out :uniform :buffer)
          for s in '(3b-spirv/hl::input
                     3b-spirv/hl::output
                     3b-spirv/hl::uniform
                     3b-spirv/hl::buffer)
          when (member i l)
            do (when interface
                 (error "multiple interfaces for object? ~s / ~s" o b))
               (setf interface s))

    (format t "~&interface binding:~%")
    (format t "  name: ~s~%" (name o))
    (format t "  layout: ~s~%" (layout-qualifier b))
    (format t "  default: ~s~%" (default b))
    (format t "  interface: ~s~%" (interface-qualifier b))
    (cond
      ((or (interface-block b) (typep (binding b) 'bindings))
       (format t "  (or interface-block bindings)~%")
       (format t "  bindings: ~s~%" (bindings (or (interface-block b)
                                                  (binding b))))
       (format t "  array-suffix: ~s~%" (array-size b)))
      (t
       (format t "  !(or interface-block bindings)~%")
       (format t "  type: ~s~%" (name (value-type b)))
       (format t "  array-suffix: ~s~%" (when (typep (value-type b) 'array-type)
                                          (array-size (value-type b))))))
    (let ((spv (list interface
                     (or (glsl-name o) (name o))
                     (name (value-type b))
                     :default (default b))))
      (format t "==~s~%" spv)
      (when (or (default b) (layout-qualifier b)
                (when (typep (value-type b) 'array-type)
                  (array-size (value-type b))))
        (break "todo" o))
      (format t "~%")
      (add-spirv spv)
      nil)))

(defmethod dump-spirv ((object constant-binding))
  (break "dump" object))

(defun spv-tmp ()
  (gensym "%"))

(defmethod dump-spirv ((o variable-read))
  (let* ((v (binding o))
         (b (stage-binding v))
         (bt (binding b))
         (tmp (spv-tmp)))
    (add-spirv `(spirv-core:load ,tmp
                                 ,(name bt)
                                 ,(name v)))
    tmp))

(defun cast (x from to)
  (format t "~&!!!cast ~s: ~s -> ~s~%"
          x from to)
  ;; possibly should assert vectors have same size?
  (let ((tmp (spv-tmp)))
    (with-matcher m (from to)
      ;; int -> float = convert-s-to-f
      ;; uint -> float = convert-u-to-f
      ;; float -> float = f-convert
      ;; int/uint -> int/uint = [us]-convert
      ;; not sure if needed since no implicit casts?
      ;; float -> int/uint = convert-f-to-[su]
      (cond
        ((numberp from)) ;; don't (or at least shouldn't?) need to cast literals
        ((or (m signed floating)
             (m signed-vector floating-vector))
         (add-spirv `(spirv-core:convert-s-to-f ,tmp ,to ,x)))
        ((or (m unsigned floating)
             (m unsigned-vector floating-vector))
         (add-spirv `(spirv-core:convert-u-to-f ,tmp ,to ,x)))
        ((or (m floating floating)
             (m floating-vector floating-vector))
         (add-spirv `(spirv-core:f-convert ,tmp ,to ,x)))
        ((or (m signed integral)
             (m signed-vector integral-vector))
         (add-spirv `(spirv-core:s-convert ,tmp ,to ,x)))
        ((or (m unsigned integral)
             (m unsigned-vector integral-vector))
         (add-spirv `(spirv-core:u-convert ,tmp ,to ,x)))
        (t (error "can't cast ~s to ~s?" from to))))
    tmp))

(defmethod dump-spirv ((o variable-write))
  (let ((tmp (dump-spirv (value o)))
        (from (if (typep (value o) 'function-call)
                  (name (first (gethash (value o) *binding-types*)))
                  (name (value-type (value o)))))
        (to (name (value-type (binding o)))))
    (unless (eq from to)
      (setf tmp (cast tmp from to)))
    (add-spirv `(spirv-core:store ,(name  (binding (binding o))) ,tmp)))
  nil)

(defmethod dump-spirv ((o global-function))
  (let ((name (name o))
        (ret (name (value-type o)))
        (body (body o))
        (bindings (bindings o))
        (binding-types (final-binding-type-cache o)))
    ;; not sure if returning arrays is valid? not handled yet if so...
    (assert (not (typep (value-type o) 'array-type)))
    (let ((body-spv (with-spirv-output ()
                      (map nil 'dump-spirv body)
                      (get-spirv))))
      (add-spirv
       `(defun ,name ,(mapcar 'name bindings)
          (declare (values ,ret)
                   ,@(loop for b in bindings
                           collect `(type ,(name (gethash b binding-types)) ,b)))
          ;; todo: local vars
          ,@body-spv))))
  nil)

(defmethod dump-spirv ((o number))
  ;; just return a THE form as the name, and let assembler handle it
  ;; todo: add COERCE if needed
  `(the ,(name *current-arg-type*) ,o))

(defmethod dump-spirv ((o function-call))
  (let* ((function-types (gethash o *binding-types*))
         (args
           (loop for a in (arguments o)
                 for *current-arg-type* in (cdr function-types)
                 for vt = (etypecase a
                            ((or number function-call)
                             *current-arg-type*)
                            (place (value-type a)))
                 when (eq (name vt) (name *current-arg-type*))
                   collect (dump-spirv a)
                 else
                   collect (cast (dump-spirv a)
                                 (name vt) (name *current-arg-type*)))))
    (dump-spirv-call (called-function o) o args)))


(defmethod generate-output (objects inferred-types
                            (backend (eql :spirv))
                            &key &allow-other-keys)
                                        ;(break "spirv" shaken inferred-types)
  (with-spirv-output ()
    (loop with dumped = (make-hash-table)
          for object in objects
          for stage-binding = (stage-binding object)
          for interface-block = (when stage-binding
                                  (interface-block stage-binding))
          unless (and interface-block (gethash interface-block dumped))
            do (etypecase object
                 ((or generic-type interface-binding constant-binding)
                  (dump-spirv object)
                  (when interface-block
                    (setf (gethash interface-block dumped) t)))
                 (global-function
                  (let ((overloads (gethash object inferred-types)))
                    (assert overloads)
                    (loop for overload in overloads
                          for *binding-types*
                            = (gethash overload
                                       (final-binding-type-cache
                                        object))
                          do (assert *binding-types*)
                             (dump-spirv object))))))
    ;; fixme: better way of communicating entry point name
    (let ((main (name *print-as-main*)))
      (print (list*
              ;; add GLSL boilerplate
                                        ;'(spirv-core:capability :todo)
              '(spirv-core:capability :shader)
              '(spirv-core:ext-inst-import :glsl "GLSL.std.450")
              '(spirv-core:memory-model :logical :glsl-450)
              `(spirv-core:entry-point ,*current-shader-stage*
                                       ,main ,main
                                       ,@(get-globals))
              ;; todo: execution-mode declarations
              `(spirv-core:execution-mode ,main :origin-lower-left)
              ;; ?
              ;; `(spirv-core:source ?? 1)
              (get-spirv))))))


#++
(3bgl-shaders::generate-stage :fragment 'skybox-shaders::fragment :backend :spirv)

#++
(3bgl-shaders::generate-stage :compute '3bgl-gpuanim-shaders::update-anim :backend :spirv)

#++
(3b-spirv/hl::assemble-to-file
 "/tmp/3bgl-shader.spv"
 (3bgl-shaders::generate-stage :fragment '3bgl-shader-example-shaders::fragment :backend :spirv))

#++
(print
 (3b-spirv::disasm
  (3b-spirv/hl::assemble
      (3bgl-shaders::generate-stage :fragment
       '3bgl-shader-spirv-test-shaders::fragment
                                    :backend :spirv))))
#++
(3bgl-shaders::generate-stage :fragment
                              '3bgl-shader-spirv-test-shaders::fragment
                              :backend :spirv)
