(in-package #:3bgl-shaders)

;; extra declarations for functions
;;   IN,OUT,INOUT for function args
;;   VALUES to declare return type of function


;;; special forms:

;; block/return-from -- not sure we can do this properly without a GOTO
;;   might be able to get close with extracting blocks to separate
;;   functions and using RETURN + special return values to mark returns
;;   through nested scopes, but probably not worth the effort...
;;   - for now, just using `(RETURN)` or `(RETURN-FROM name-of-function)`
;;     as glsl `return` which exits entire function
;; throw/catch -- ignore
;; load-time-value -- ignore?
;; locally -- probably ignore? since we can't have variant types
;;    in generated code, LOCALLY as best just makes type inference more
;;    confusing...
;; function -- ignore?
;;    not sure we can have function pointers, so ignore for now...
;; multiple-value-call -- ignore
;; multiple-value-prog1 -- ignore
;; progv -- ignore
;; quote -- ignore? (or expand during earlier pass if any meaningful uses?)
;;

;; unwind-protect -- ?

;; eval-when -- remove during early pass (eval compile time stuff, leave
;;    load/execute stuff in code without eval-when)
;; symbol-macrolet -- expanded in earlier passes
;; macrolet -- expanded in earlier passes
;; flet/labels -- inline during earlier pass?
;;   (or possibly extract to separate functions?)
;;   can't have recursion though, so probably doesn't matter too much

;;; CL special forms included in IR

;; let
;; let*
;; setq
;; tagbody (probably best to avoid since it won't expand well)

;;; extra special forms
;; DO or DO*
;;   possibly should just expand to FOR?
;; FOR
;;   not sure if this should be exposed at CL level, or just
;;   be there for LOOP/DO expansions?
;;   hard to come up with a nice syntax to cover possiblity of mixing bindings
;;     with non-binding expressions in init/test clauses?
;; DO-WHILE / WHILE-DO ?
;;   similar issues to FOR, but might be worth adding a no-bindings
;;   form anyway?
;; SWITCH ?
;;
;; RETURN/BREAK/CONTINUE/DISCARD
;;   could probably pretend these are just builtin functions, but
;;   might eventually want to be able to distinguish them for dead
;;   code elimination etc


;; intermediate representation:
;;
;;; objects/mixins
;;
;;  binding-scope mixin
;;    bindings for a DEFUN or LET or LET* or FOR loop, set of bindings
;;      + types + initial values
;;
;;  variable write
;;  variable read
;;    local, global (varying, uniform, ?)
;;  function call
;;    builtin, global, ?
;;      might want to distinguish between stuff like + that is
;;      expanded by specially compiler, and other builtin functions
;;      that expand to actual function calls?
;;
;;  defun
;;
;;  global var binding?
;;  uniforms?
;;  varyings?

(defclass place ()
  ())

(defclass binding (place)
  ((name :accessor name :initarg :name)
   (glsl-name :accessor glsl-name :initarg :glsl-name :initform nil)
   ;; type of value stored in this binding (possibly not concrete if
   ;; type inference not done yet, or if function is still generic)
   (value-type :accessor value-type :initarg :value-type)
   (qualifiers :accessor qualifiers :initform nil)))

(defclass initialized-binding (binding)
  ;; for actual variables in the code (global or local)
  ((initial-value-form :accessor initial-value-form :initarg :init)))

(defclass variable-binding (initialized-binding)
  ())

(defclass constant-binding (initialized-binding)
  ((internal :accessor internal :initform nil)))

(defmethod initialize-instance :after ((i constant-binding)
                                       &key &allow-other-keys)
  (pushnew :const (qualifiers i)))

;; do we need to distinguish locals from globals?
(defclass local-variable (variable-binding)
  ())

(defclass symbol-macro (binding)
  ((expansion :accessor expansion :initarg :expansion)))

;; not sure if this needs to be distinct?
(defclass function-argument (local-variable)
  ())


(defclass progn-body () ;; mixin for forms with (implicit) progn
  ;; possibly should have a subclass for explicit progn, so we can
  ;; distinguish that from implicit progns? (so we can do things
  ;; like merging nested progns)
  ;; alternately, might be better to just make all progns explicit?
  ((body :accessor body :initarg :body)))

(defclass explicit-progn (progn-body)
  ())
(defclass implicit-progn (progn-body)
  ())

(defclass bindings ()
  ;; list (sequence?) of bindings corresponding to a let scope,
  ;; function arglist, etc
  ((bindings :accessor bindings :initarg :bindings)))

(defclass binding-scope (bindings implicit-progn)  ;; let/let*
  ((declarations :initarg :declarations :accessor declarations)))


(defclass function-binding ()
  ;; bindings in function namespace
  ((name :accessor name :initarg :name)
   (glsl-name :accessor glsl-name :initarg :glsl-name :initform nil)
   (declarations :initarg :declarations :accessor declarations)
   (docs :initarg :docs)
   ;; might just distinguish them by type of the instance instead?
   #++(binding-type :accessor binding-type :initarg :binding-type)))

(defclass function-binding-function (function-binding)
  ;; possibly should also store some sort of type-inference function(s)
  ;; with these? (for example to specify all args have to be vectors
  ;; of same types, or whatever, or figure out return type of something
  ;; like `outer-product` or `transpose` from arg types
  ((return-type :accessor return-type :initarg :return-type)
   ;; sexp lambda list (with &key, etc)
   ;; (no &rest though, since we don't have lists)
   (lambda-list :initarg :lambda-list)
   ;; c-style lambda list, only positional/optional args
   ;; optional as (name value)?
   ;; (arglist :initarg :arglist) -- just using BINDINGS for now?
   ;; to support &key args, we optionally have a sort of compiler-macro
   ;; associated with functions, to expand keyword args in the source
   ;; into positional args
   ;; todo: check for reordering argument values with side effects and warn
   ;;  (since we might epand something like (foo :a a :b b) into (foo b a)
   ;;   which could matter if A or B modify the same variable, or one
   ;;   modifies a variable the other depends on)
   ;; todo: compile this lazily like macros? (and maybe combine with them?)
   (expander :accessor expander :initarg :expander :initform #'identity)
)
)

(defclass global-function (function-binding-function implicit-progn bindings)
  ;; global function definitions, with function body
  ())

(defclass builtin-function (function-binding-function bindings)
  ;; declarations for functions provided by glsl
  ;; (or external glsl code)
  ())

(defclass internal-function (function-binding-function bindings)
  ;; like a builtin function, but we compile it specially
  ;; for example AND -> &&, etc
  ;; probably mostly CL functions?
  ())

;; we also have macros and local functions during early passes..
(defclass macro-definition (function-binding)
  ;; we store expression to compile for macro, and only compile the
  ;; actual macro function the first time it is used
  ((expression :accessor expression :initarg :expression )
   (expander :accessor expander :initform nil)))

;; not sure we need to distinguish global from local macros, since local
;; macros shoul only exist in transient environment scopes?
#++
(defclass global-macro (macro-definition)
  ())
#++
(defclass local-macro (macro-definition)
  ())


;;; not supporting bindings in FOR loops for now, since they aren't
;;; really very general (we can bind multiple variables in
;;; 'init-expression' but only if they are all the same type, and we
;;; can't mix bindings and non-binding expressions, etc)
;;; instead we can just wrap it in a LET and let it expand to
;;   `{ <bindings> for (...) {...} }`
;;; and if we really want to, later expand the simple cases to
;;; `for (<binding>...) {...}` if we care about the generated code
(defclass for-loop (implicit-progn)
  ((init-form :accessor init-form)
   (condition-form :accessor condition-form)
   (step-form :accessor step-form)))


;; slot/array access are used like bindings for now, might need to be
;; smarter once we start type inference?
(defclass slot-access (place)
  ((binding :accessor binding :initarg :binding)
   (field :accessor field :initarg :field)))
(defclass array-access (place)
  ((binding :accessor binding :initarg :binding)
   (index :accessor index :initarg :index)))



(defclass variable-read (place)
  ;; possibly should store some type info as well?
  ((binding :accessor binding :initarg :binding)))

(defclass variable-write ()
  ;; possibly should store some type info as well?
  ((binding :accessor binding :initarg :binding)
   (value :accessor value :initarg :value)))

(defclass function-call ()
  ((called-function :accessor called-function :initarg :function)
   (arguments :accessor arguments :initarg :arguments)))

(defclass if-form ()
  ((test-form :accessor test-form :initarg :test)
   (then-form :accessor then-form :initarg :then)
   (else-form :accessor else-form :initarg :else))
)



;;; these don't assign results of walking children back into parent, since
;;; other objects might refer to them... if we really need to make changes,
;;; try change-class or something?

(defmethod walk ((form initialized-binding) walker)
  (walk (initial-value-form form) walker)
  (when (next-method-p)
    (call-next-method)))

(defmethod walk ((form function-call) walker)
  (loop for i in (arguments form)
        do (walk i walker))
  (when (next-method-p)
    (call-next-method)))

(defmethod walk ((form progn-body) walker)
  (loop for i in (body form)
        do (walk i walker))
  (when (next-method-p)
    (call-next-method)))

(defmethod walk ((form bindings) walker)
  (loop for i in (bindings form)
        do (walk i walker))
  (when (next-method-p)
    (call-next-method)))

(defmethod walk ((form array-access) walker)
  (walk (binding form) walker)
  (walk (index form) walker)
  (when (next-method-p)
    (call-next-method)))

(defmethod walk ((form slot-access) walker)
  (walk (binding form) walker)
  (when (next-method-p)
    (call-next-method)))

(defmethod walk ((form variable-read) walker)
  (walk (binding form) walker)
  (when (next-method-p)
    (call-next-method)))

(defmethod walk ((form variable-write) walker)
  (walk (binding form) walker)
  (walk (value form) walker)
  (when (next-method-p)
    (call-next-method)))

(defmethod walk ((form if-form) walker)
  (walk (test-form form) walker)
  (walk (then-form form) walker)
  (walk (else-form form) walker))


