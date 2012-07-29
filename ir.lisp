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


(defclass binding ()
  ((name :accessor name :initarg :name)
   ;; type of value stored in this binding (possibly not concrete if
   ;; type inference not done yet, or if function is still generic)
   (value-type :accessor value-type :initarg :value-type)
   ;; type of binding (local, global, attribute, uniform, output, ?)
   ;; possibly should distinguish between builtins and user defined vars?
   (binding-type :accessor binding-type :initarg :binding-type)
   (initial-value-form :accessor initial-value-form :initarg :init)))

(defclass progn-body () ;; mixin for forms with (implicit) progn
  ((body :accessor body)))

(defclass bindings ()
  ((bindings :accessor bindings)))

(defclass binding-scope (bindings progn-body)  ;; let/let*, also used by defun
  ())

(defclass global-function (binding-scope)
  ;; possibly should also store some sort of type-inference function(s)
  ;; with these? (for example to specify all args have to be vectors
  ;; of same types, or whatever, or figure out return type of something
  ;; like `outer-product` or `transpose` from arg types
  ((name :accessor name)
   (return-type :accessor return-type)))

(defclass builtin-function (bindings)
  ;; declarations for functions provided by glsl
  ;; (or external glsl code)
  ((name :accessor name)
   (return-type :accessor return-type)))

(defclass internal-function (bindings)
  ;; like a builtin function, but we compile it specially
  ;; for example AND -> &&, etc
  ;; probably mostly CL functions?
  ((name :accessor name)
   (return-type :accessor return-type)))

;;; not supporting bindings in FOR loops for now, since they aren't
;;; really very general (we can bind multiple variables in
;;; 'init-expression' but only if they are all the same type, and we
;;; can't mix bindings and non-binding expressions, etc)
;;; instead we can just wrap it in a LET and let it expand to
;;   `{ <bindings> for (...) {...} }`
;;; and if we really want to, later expand the simple cases to
;;; `for (<binding>...) {...}` if we care about the generated code
(defclass for-loop (progn-body)
  ((init-form :accessor init-form)
   (condition-form :accessor condition-form)
   (step-form :accessor step-form)))

(defclass var-read ()
  ;; possibly should store some type info as well?
  ((binding :accessor binding)))

(defclass var-write ()
  ;; possibly should store some type info as well?
  ((binding :accessor binding)
   (value :accessor value)))

(defclass function-call ()
  ((called-function :accessor called-function)
   (arguments :accessor arguments)))





