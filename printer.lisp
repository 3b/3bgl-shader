(in-package #:3bgl-shaders)

(defparameter *pprint-glsl* (copy-pprint-dispatch nil))

(defparameter *in-expression* nil)


(defun translate-name (x)
  ;; todo: lookup name in environment to allow for specifying a translation
  (unless (stringp x)
    (setf x (string x)))
  (with-output-to-string (s)
    ;; fixme: smarter translation, filter more characters, etc
    (if (char= #\+ (char x 0) (char x (1- (length x))))
        (with-standard-io-syntax
          (format s "~:@(~a~)" (substitute #\_ #\- (remove #\+ x))))
        (loop with uc = 0
              for c across (string x)
              do (case c
                   (#\* (incf uc 1))
                   (#\- (incf uc 1))
                   (#\+
                    (incf uc 1)
                    (format s "_"))
                   (#\% (format s "_"))
                   (t (if (plusp uc)
                          (format s "~:@(~c~)" c)
                          (format s "~(~c~)" c))
                    (setf uc (max 0 (1- uc)))))))))

(defmacro assert-statement ()
  `(when *in-expression*
     (error "trying to print statement in expression context")))

(set-pprint-dispatch 'symbol
                     (lambda (s o)
                       (format s "~a" (translate-name o)))
                     0
                     *pprint-glsl*)

 ;; print uny cons without a higher-priority printer as a normal function call
(set-pprint-dispatch 'cons
                     (lambda (s o)
                       (let ((*in-expression* t))
                         (format s "~a~<(~;~@{~:_~a~#[~:;, ~]~}~;)~:>" (car o) (cdr o))))
                     0
                     *pprint-glsl*)

(defmacro defprint ((form &rest args) () &body body)
  (alexandria:with-gensyms (stream object)
    `(set-pprint-dispatch
      '(cons (member ,form))
      (lambda (,stream ,object)
        (let ((*standard-output* ,stream))
          (destructuring-bind (,@args) (cdr ,object)
            ,@body)))
      1 *pprint-glsl*)))

(defprint (:var name &key init qualifiers type) ()
  (assert-statement)
  (let ((*in-expression* t))
    (with-standard-io-syntax
      (format *debug-io* ":var ~s ~s ~s ~s~%" name init qualifiers type))
    (format t "~{~a ~}~@[~a ~]~a~@[ = ~a~]" qualifiers type name init)))

(defprint (:var name &key init qualifiers type) ()
  (assert-statement)
  (let ((*in-expression* t))
    (with-standard-io-syntax
      (format *debug-io* ":var ~s ~s ~s ~s~%" name init qualifiers type))
    (format t "~{~a ~}~@[a ~]~a~@[ = ~a~]" qualifiers type name init)))


#++
(defprint (defun name (&rest lambda-list) &body body) ()
  (assert-statement)
  (let ((ret (if (consp name) (second name) "void"))
        (name (if (consp name) (first name) name)))
    (format t "--~a ~a ~<(~;~@{~:_~a~#[~:;, ~]~}~;)~:> {~%" ret name
            lambda-list)
    (format t "~a" (cons 'progn body))
    (format t "}~%"))
  )

(defprint (:toplevel-function name &key return-type body lambda-list
                              &allow-other-keys) ()
  (assert-statement)
  (format t "~a ~a ~<(~;~@{~:_~a~#[~:;, ~]~}~;)~:> {~%"
          (or return-type "void")
          name
          lambda-list)
  (format t "~a" (cons 'progn body))
  (format t "}~%"))

(defprint (progn &rest body) ()
  (if *in-expression*
      (format t "~<(~@;~@{~a~^,~})~:>" body)
      (format t "~<  ~@;~@{~a;~^~%~}~:>~%" body)))

(defprint (if cond then else) ()
  (if *in-expression*
      (format t "(~a?~a:~a)" cond then else)
      (progn
        (let ((*in-expression* t))
          (format t "if (~a) {~%" cond))
        (format t "~a" (list 'progn then))
        (when else
          (format t "} else {~%")
          (format t "~a" (list 'progn else)))
        (format t "}"))))


(defmacro defprint-binop (op c-op 0-arg 1-arg)
  `(defprint (,op &rest args) ()
     (let ((*in-expression* t))
       (case (length args)
         (0 ,(or 0-arg `(error ,(format nil "no arguments to ~a ?" op))))
         (1 ,(or
              (if (eq 1-arg t)
                  `(format t "~a" (car args))
                  1-arg)
              `(format t ,(format nil "(~a~~a)" c-op) (car args))))
         (t (format t ,(format nil "(~~{~~a~~^ ~~#[~~:;~a ~~]~~})" c-op) args))))))

;;; fixme: probably shouldn't be allowing 0/1 arg versions of these that
;;;        use some specific number, since it should have been accounted for
;;;        before type inference?
(defprint-binop - "-" nil nil)
(defprint-binop + "+" 0.0 t)
(defprint-binop * "*" 1.0 t)
(defprint-binop / "/" 1.0 (format t "(1.0 / ~a)" (car args)))
(defprint-binop or "||" 0 t)
(defprint-binop and "&&" 1 t) ;; should this be #xffffffff instead of 1 for t?
(defprint-binop logior "|" 0 t)
(defprint-binop logand "&" #xffffffff t) ;; fixme: how many bits is -1?
(defprint-binop logxor "^" 0 t)



(defprint (1- x) ()
  (let ((*in-expression* t))
    (format t "~a" (- x 1))))
(defprint (1+ x) ()
  (let ((*in-expression* t))
    (format t "~a" (+ x 1))))

;; only handling binary versions of compare ops for now,
;; can expand multi arg versions in earlier pass if needed
(macrolet ((compares (&rest ops)
             `(progn
                ,@(loop for (op c-op) in ops
                        collect `(defprint (,op a b) ()
                                   (let ((*in-expression* t))
                                     (format t ,(format nil"(~~a ~a ~~a)" c-op)
                                             a b)))))))
  (compares (= "==")
            (/= "!=")
            (< "<")
            (> ">")
            (<= "<=")
            (>= ">=")))


(defprint (ash i c) ()
  ;; if we can't tell in advance which direction we shift, we need to
  ;; check at runtime... probably should add << and >> operators for
  ;; fixed direction too.
  (cond
    ((numberp c)
     (let ((*in-expression* t))
       (format t "(~a ~a ~a)" i (if (plusp c) "<<" ">>") c)))
    (t (format t "~a" `(if (< ,c 0)
                           ,(let ((*in-expression* t))
                              (format nil "(~a >> ~a)" i c))
                           ,(let ((*in-expression* t))
                              (format nil "(~a << ~a)" i c))))))) 





(defun pprint-glsl (form)
  (let* ((*print-pprint-dispatch* *pprint-glsl*)
         (*print-pretty* t)
         (old-debug *debugger-hook*)
         (*debugger-hook* (lambda (&rest r)
                            (with-standard-io-syntax
                              (apply old-debug r)))))
    (format t "~a~%" form)))