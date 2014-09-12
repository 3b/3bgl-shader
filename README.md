### 3bgl-shader: a Common Lisp DSL for generating GLSL shaders

### Features

* looks more-or-less like CL
* type inference
* hooks for interactive use
* automatic overloaded functions

### Minimal Example shader program

Just transform the vertex, and send color to be interpolated.
(A longer (if inefficient) example can be found [here](https://github.com/3b/3bgl-misc/blob/master/skybox/shaders.lisp).)

```Lisp
;; define a package for the shader functions, :USEing :3bgl-glsl and :cl
(cl:defpackage #:shader
  (:use :cl :3bgl-glsl)
  ;; we want DEFUN and DEFCONSTANT from #:3bgl-glsl rather than CL though
  (:shadowing-import-from #:3bgl-glsl #:defun #:defconstant))
(cl:in-package #:shader)

;; vertex attributes, need to specify types for all 'external'
;; interfaces and globals (inputs, outputs, uniforms, varyings)
(input position :vec4 :location 0)
(input color :vec4 :location 1)

;; final output
(output out-color :vec4 :stage :fragment)

;; model-view-projection matrix
(uniform mvp :mat4)

;; interface between vertex and fragment shader
(interface varyings (:out (:vertex outs)
                     :in (:fragment ins))
  (color :vec4))

;; vertex shader (names are arbitrary)
(defun vertex ()
  (setf (@ outs color) color
        gl-position (* mvp position)))

;; fragment shader
(defun boring-fragment ()
  (setf out-color (@ ins color)))

 ;; not quite CL:defconstant, need to specify a type
(defconstant +scale+ 2.0 :float)
;; a helper function
(defun invert-and-scale (x)
  ;; RETURN is GLSL return rather than CL:RETURN, and is required for
  ;; functions that return a value
  (return (* +scale+ (- 1 x))))

;; an alternate fragment shader
(defun inverted-fragment ()
  (setf out-color (invert-and-scale (@ ins color))))
```

#### Convert to glsl

```Lisp
(3bgl-shaders:generate-stage :vertex 'shader::vertex)
(3bgl-shaders:generate-stage :fragment 'shader::boring-fragment)
(3bgl-shaders:generate-stage :fragment 'shader::inverted-fragment)
```

results:
```glsl
// vertex shader:
#version 450
out varyings {
  vec4 color;
} outs;

layout(location = 1) in vec4 color;

uniform mat4 mvp;

layout(location = 0) in vec4 position;

void main () {
  outs.color = color;
  gl_Position = (mvp * position);
}

// boring fragment shader:
#version 450
out vec4 outColor;

in varyings {
  vec4 color;
} ins;

void main () {
  outColor = ins.color;
}

// inverted fragment shader
#version 450
out vec4 outColor;

const float SCALE = 2.0;
in varyings {
  vec4 color;
} ins;

vec4 invertAndScale (vec4 x) {
  return (SCALE * (1 - x));
}

void main () {
  outColor = invertAndScale(ins.color);
}

```


### Hooks for interactive use

Programs using 3bgl-shader can add a function to
`3bgl-shaders::*modified-function-hook*`, which will be called when
shader functions are redefined. It will be passed a list of names of
updated functions. For example in the shaders above, if the
`(defconstant +scale+ 2.0 :float)` form were recompiled in slime with
`C-c C-c`, the hook functions would be passed the list
`(SHADER::INVERTED-FRAGMENT SHADER::INVERT-AND-SCALE)` since
`invert-and-scale` depends on the constant, and `inverted-fragment`
depends on the function `invert-and-scale`.  The hook function could
then see one of the fragment shaders it is using had been modified,
and arrange for a running program to try to recompile the shader
program next frame.


### Current status

The compiler and type inference mostly work, including some fairly
complicated shaders.

Built-in functions/types/variables from glsl version 4.50 are
available, and older versions might work to the extent they are
compatible with the subset of 4.50 used in a particular shader.  The
type inference doesn't distinguish between versions, so might allow
casts that wouldn't be valid in an older version.

Error messages are mostly horrible, so most type inference failures
will give incomprehensible errors.

CL style type declarations are allowed and should mostly be respected.

API isn't completely finished, so some parts may change (in particular
the base types like `:vec3` may be renamed to `3bgl-glsl:vec3` at some
point.)

The external API needs more work, in particular some way to query
uniforms, inputs, outputs, etc.

Currently no way to translate line/column numbers from glsl error
messages back to source.

Performance is acceptable for shaders I've tested it on, but not sure
how it scales. It currently `WARN`s if it takes more than 2000 passes
for type inference, which may need adjusted or disabled for larger shaders.

Currently all functions that depend on a function/global will be
recompiled when things they depend on are recompiled, which can make
changing function signatures or types difficult if they aren't
compatible with the uses.

Recompilation may be more aggressive than it needs to be, for
example if the value of a constant is changed, it shouldn't need to
re-run type inference of functions that use that constant if the type
didn't change.

### Misc notes

#### Concrete types

GLSL types are currently named with keywords (though that may change
in the future), like `:vec2`, `:vec3`, `:vec4`, `:mat2x4`,
`:sampler-2d-array-shadow` etc.  see [the
source](https://github.com/3b/3bgl-shader/blob/master/types.lisp#L356-L476)
for details for now, though most are fairly obvious.

#### Component swizzles

Components of GLSL vector types like `:vec4` can be accessed with
'swizzle' functions like `.xyz`, so for example glsl `someVec.rraa`
would be `(.rraa some-vec)`. Type inference should correctly use the
swizzle to determine minimum size of the vector if not specified.

#### Structure/interface slots

`(@ var slot-name)` is a shortcut for `(slot-value var 'slot-name)`,
and either will compile to `var.slot`. GLSL doesn't allow specifying a
slot through a variable, so slot name must be a quoted compile-time
literal.

#### RETURN

Functions are required to use `RETURN` to return values, the will not
return the value of the last form as in CL.  A function without a
`RETURN` will have a `void` return type.  `(return (values))` can also
be used to force a `void` return type, and for early exit from a
`void` function.

#### Overloaded functions

If a function doesn't have a specific derived or specified type, it
can be used with any compatible types, and the generated GLSL will
have a version for each type.

For example the previous code could have had

```Lisp

 ;; X can be any type that works with scalar `*` and `-`
(defun invert-and-scale (x)
  (return (* +scale+ (- 1 x))))

(defun inverted-fragment ()
  (setf out-color (+ (invert-and-scale 1) ;; call 'int' version
                     (invert-and-scale (@ ins color))))) ;; call 'vec4' version
```

which would generate the glsl code

```glsl
#version 450
out vec4 outColor;

const float SCALE = 2.0;
in varyings {
  vec4 color;
} ins;

// returns a vec4 because the input is vec4
vec4 invertAndScale (vec4 x) {
  return (SCALE * (1 - x));
}

// returns float because SCALE is a float
float invertAndScale (int x) {
  return (SCALE * (1 - x));
}

void main () {
  outColor = (invertAndScale(1) + invertAndScale(ins.color));
}
```

#### Type declarations

CL-style type declarations are allowed, and should interact correctly
with type inference.

for example

```Lisp
(defun foo (x y)
  (declare (values :float) (:float x))
  (let ((a (+ x y)))
     (declare (:vec2 a))
     (return (.x a))))
```

specifies that `foo` returns a `float`, the first argument is also
specified to be `float`, while the second isn't explicitly
restricted. The local variable `A` is specified to be a `vec2`, which
implicitly restricts `Y` to also be something that casts to `vec2`.

`(declare (values))` can be used to explicitly specify `void` return
type for a function.


#### Uniforms, input, output, interface

Uniforms are specified with `(UNIFORM name type &key stage)` where
`stage` specifies in which shader stages (`:vertex`,`:fragment` etc)
the uniform is visible (by default the uniform is visible in all
stages, though will only be included in generated GLSL for stages in
which it is referenced).

Inputs and outputs are specified with `(INPUT name type &key stage location)`
and `(OUTPUT name type &key stage location)`
where `stage` specifies in which shader stages (`:vertex`,`:fragment`
etc) the input is visible, and `location` is an integer which will be
output as `layout(location = 1)` in GLSL.

Interfaces between stages are specified as `(INTERFACE name (&key in
out uniform) &body slots)`. `slots` is a list of `(slot-name
type)`. `in`, `out` and `uniform` specify how the interface will be
visible, and are either `T` to make it visible to all stages as
`name`, or a plist of stage names and names to use for the interface in that stage.

For example `(interface varyings (:out (:vertex outs) :in (:fragment
ins :geometry (ins "ins" :*))) ...)` will be visible as an output
named `out` in the vertex shader, as an input array named `ins` in the
geometry shader, and as an input named `ins` in the fragment shader.


`name` and `slot-name` in uniform/input/output/interface can either be
a symbol which will be automatically converted from `lisp-style` to
`glslStyle`, or it can be a list of `(lisp-name "glslName")` to
provide an explicit translation.