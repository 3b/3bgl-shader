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

(defmethod name (o)
  o)

(defclass binding (place)
  ((name :accessor name :initarg :name)
   (glsl-name :accessor glsl-name :initarg :glsl-name :initform nil)
   ;; inferred type of variable or T if not known yet
   (value-type :accessor value-type :initarg :value-type
               :initform t)
   ;; most things allow implicit casts, but constructors explicitly allow
   ;; a larger set of other types, so we don't want to confuse things
   ;; by allowing casts in addition to that
   ;; (not sure if it affects correctness or not, but should be faster
   ;;  and maybe reduce ambiguity)
   (allow-casts :accessor allow-casts :initform t :initarg :allow-casts)
   ;; T for unknown, or a type or a binding object to share a type
   ;; with another binding (for example functions that accept any type
   ;; as long as all arguments are same type, or where return type
   ;; matches an arg type)
   (declared-type :accessor declared-type :initarg :declared-type
                  :initform t)
   (qualifiers :accessor qualifiers :initform nil :initarg :qualifiers)
   ;; set to true if variable needs renamed due to a scoping conflict
   ;; (for example shadows a variable used in initialization of
   ;; variable in same scope)
   (conflicts :accessor conflicts :initform nil)))

(defclass binding-with-dependencies ()
  ;; we need to track dependencies to allow automatic recompiling, but
  ;; not for all bindings (builtin functions/variables shouldn't be
  ;; changing for example, so we don't want them holding references to
  ;; old functions forever)
  ((bindings-used-by :reader bindings-used-by :initform (make-hash-table))
   (bindings-using :reader bindings-using :initform (make-hash-table))))

(defclass initialized-binding (binding)
  ;; for actual variables in the code (global or local)
  ((initial-value-form :accessor initial-value-form :initarg :init)))

(defclass variable-binding (initialized-binding)
  ())

(defclass constant-binding (initialized-binding  binding-with-dependencies)
  ((internal :accessor internal :initform nil)))

(defmethod initialize-instance :after ((i constant-binding)
                                       &key &allow-other-keys)
  (pushnew :const (qualifiers i)))

(defclass local-variable (variable-binding)
  ())

(defclass global-variable (variable-binding binding-with-dependencies)
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
   ;; inferred type of variable or T if not known yet
   (value-type :accessor value-type :initarg :value-type
               :initform t)
   ;; T for unknown, or a type or a binding object to share a type
   ;; with another binding (for example functions that accept any type
   ;; as long as all arguments are same type, or where return type
   ;; matches an arg type)
   (declared-type :accessor declared-type :initarg :declared-type
                  :initform t)))

(defclass function-binding-function (function-binding binding-with-dependencies)
  ;; todo: add some way to detect changes in type inference data
  ;; so we don't need to redo type inference for callers if not needed?
  ;; not sure how often they will actually be similar enough though,
  ;; so might not actually be worth it?
  (;; nil = not run yet, t = OK, :failed = unknown globals or type
   ;; conflicts (possibly should also distinguish between resolved to
   ;; single types or not?)
   (type-inference-state :initform nil :accessor type-inference-state
                         :initarg :type-inference-state)
   ;; T or list of names of stages in which the function is valid
   ;; (not automatically updated when things are defined in more stages
   ;;  that might make it valid, so possibly should try recompiling if
   ;;  used from a different stage)
   ;; used by type inference to decide which stages to run inference for
   ;; fixme: is this actually used/useful?
   (valid-stages :initform t :accessor valid-stages :initarg :valid-stages)
   ;; 'type' object for each local binding inside the function
   ;; including arguments (keyed by binding object)
   ;; and return value (keyed by :return)
   ;; as well as an entry for every call to a global-function
   ;; (keyed by an inference-call-site object or something like that
   ;;  with list of types (ret then supplied arg types) as value)
   ;; only valid after type inference is run, should be empty otherwise
   ;; probably not concrete types for most functions, since types
   ;; depend on argument types. (specific types are chosen during final
   ;; compile for any sets of argument types actually used)
   (local-binding-type-data :initform (make-hash-table)
                            :accessor local-binding-type-data)
   ;; concrete type data for any overloaded versions of this function
   ;; which have been computer so far, keyed by a list of concrete types
   ;; value is hash table like local-binding-type-data, except with
   ;; a list of lists of arg types for inference-call-site entries
   ;; (only calculated as needed, so only contains data for combinations
   ;;  actually compiled since function was last modified)
   (final-binding-type-cache :initform (make-hash-table :test 'equal)
                             :accessor final-binding-type-cache)
   ;; sexp lambda list (with &key, etc)
   ;; (no &rest though, since we don't have lists)
   (lambda-list :initarg :lambda-list :accessor lambda-list)
   (old-lambda-list :initform t :accessor old-lambda-list)
   ;; to support &key args, we optionally have a sort of compiler-macro
   ;; associated with functions, to expand keyword args in the source
   ;; into positional args
   ;; todo: check for reordering argument values with side effects and warn
   ;;  (since we might epand something like (foo :a a :b b) into (foo b a)
   ;;   which could matter if A or B modify the same variable, or one
   ;;   modifies a variable the other depends on)
   ;; todo: compile this lazily like macros? (and maybe combine with them?)
   (expander :accessor expander :initarg :expander :initform #'identity)
   ;; 'layout' qualifiers for shader with this function as 'main'
   (layout-qualifiers :accessor layout-qualifiers :initform (make-hash-table))))

(defun function-signature-changed (fun)
  ;; todo: ignore changed names (but keep changed defaults, so can't
  ;; just look at shape of tree)
  (not (equal (lambda-list fun) (old-lambda-list fun))))

#++
(defun function-type-changed (fun)
  (not (equal (function-type fun) (old-function-type fun))))

(defclass global-function (function-binding-function implicit-progn bindings)
  ;; global function definitions, with function body
  ())

(defclass unknown-function-binding (function-binding binding-with-dependencies)
  ;; reference to an unknown function, will be CHANGE-CLASSed to
  ;;    function-binding-function when defined
  ;; store onvironment that was current when the reference was made,
  ;;   so we can check there, will also check for an environment in
  ;;   symbol-package of function's name
  ;; (probably should be in package's
  ((environment :accessor environment :initarg :environment)))

(defclass builtin-function (function-binding-function bindings)
  ;; declarations for functions provided by glsl
  ;; (or external glsl code)
  ())

(defclass internal-function (function-binding-function bindings)
  ;; like a builtin function, but we compile it specially
  ;; for example AND -> &&, etc
  ;; probably mostly CL functions?
  ()
  (:default-initargs :type-inference-state t))

;; we also have macros and local functions during early passes..
(defclass macro-definition (function-binding)
  ;; we store expression to compile for macro, and only compile the
  ;; actual macro function the first time it is used
  ((expression :accessor expression :initarg :expression)
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
  ((init-forms :accessor init-forms :initarg :init)
   (condition-forms :accessor condition-forms :initarg :while)
   (step-forms :accessor step-forms :initarg :step)))


;; slot/array access are used like bindings for now, might need to be
;; smarter once we start type inference?
(defclass slot-access (place)
  ((binding :accessor binding :initarg :binding)
   (field :accessor field :initarg :field)
   (value-type :accessor value-type :initarg :value-type)))
(defclass swizzle-access (place)
  ((binding :accessor binding :initarg :binding)
   (field :accessor field :initarg :field)
   (min-size :accessor min-size :initarg :min-size)
   (value-type :accessor value-type :initarg :value-type)))
(defclass array-access (place)
  ((binding :accessor binding :initarg :binding)
   (index :accessor index :initarg :index)
   (value-type :accessor value-type :initarg :value-type)))
(defmethod name ((o slot-access))
  (name (binding o)))
(defmethod name ((o swizzle-access))
  (list (name (binding o)) (field o)))
(defmethod name ((o array-access))
  (list (name (binding o)) '[ (index o) ']))


(defclass variable-read (place)
  ;; possibly should store some type info as well?
  ((binding :accessor binding :initarg :binding)))
(defmethod name ((o variable-read))
  (name (binding o)))
(defmethod value-type ((o variable-read))
  (value-type (binding o)))

(defclass variable-write ()
  ;; possibly should store some type info as well?
  ((binding :accessor binding :initarg :binding)
   (value :accessor value :initarg :value)))
(defmethod name ((o variable-write))
  (name (binding o)))

(defclass function-call ()
  ((called-function :accessor called-function :initarg :function)
   (arguments :accessor arguments :initarg :arguments)
   ;; we need to store enough information to recompile
   ;; calls when called function is (re)defined, since we expand
   ;; &key and such at the call site
   (raw-arguments :accessor raw-arguments :initarg :raw-arguments)
   (argument-environment :accessor argument-environment
                         :initarg :argument-environment)))
(defmethod name ((o function-call))
  (name (called-function o)))

;; hack for array initialization, (vector a b c ...) prints as {a,b,c,...}
(defclass array-initialization (array-type)
  ((arguments :accessor arguments :initarg :arguments)
   (raw-arguments :accessor raw-arguments :initarg :raw-arguments)
   (argument-environment :accessor argument-environment
                         :initarg :argument-environment)))
(defclass if-form ()
  ((test-form :accessor test-form :initarg :test)
   (then-form :accessor then-form :initarg :then)
   (else-form :accessor else-form :initarg :else)))



;;; these don't assign results of walking children back into parent, since
;;; other objects might refer to them... if we really need to make changes,
;;; try change-class or something?

(defmethod walk ((form initialized-binding) walker)
  (walk (initial-value-form form) walker)
  (when (next-method-p)
    (call-next-method)))

(defmethod walk ((form array-initialization) walker)
  (loop for i in (arguments form)
        do (walk i walker))
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
