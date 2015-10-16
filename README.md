
# salt

A [Chicken Scheme](http://www.call-cc.org/) library for equation-based
modeling and simulations.

## Usage

salt [options...] [input files ...]

## Introduction


{{SALT}} is an implementation of a domain-specific language for
equation-based model. This implementation follows the work of Tom
Short and the Julia Sims.jl library
(http:https://github.com/tshort/Sims.jl), which is in turn based on
David Broman's MKL  simulator and the work of George Giorgidze and
Henrik Nilsson in functional hybrid modeling.  Following
Sims.jl, a nodal formulation is used based on David Broman's
thesis (http://www.bromans.com/david/publ/thesis-2010-david-broman.pdf) :  

```
David Broman. Meta-Languages and Semantics for
Equation-Based Modeling and Simulation. PhD thesis, Thesis No
1333. Department of Computer and Information Science, Linköping
University, Sweden,; 2010. 
```




## Library procedures

```
parse :: DECLARATIONS -> ASTDECLS
```

Parses equation declarations in the syntax described in the next
section, and returns an abstract syntax tree structure.

```
elaborate :: ASTDECLS -> EQUATIION-SET
```

Performs flattening of the given declarations and returns an equation set.
The main steps in flattening are:

- Creation of a name resolution environment (parameters,fields,externals,constants,variables,functions).
- Replacing of fixed initial values.
- Flattening models and populating equation, definition, function lists.
- Populating list of initials.
- Populating event list from event and structural event definitions.
- Handles structural events.

```
simcreate :: EQUATION-SET -> SIMRUNTIME
```

Given an equation set, creates a simulation runtime representation.


```
codegen-ODE :: SIMRUNTIME -> ODE LIST
```

Given a simulation runtime representation, creates an abstract code
representation aimed at ODE solvers.


```
codegen-ODE/ML
```    

Given a simulation runtime representation, creates a code
representation aimed at ODE solvers in the Standard ML language.

## Model description language

### Definitions

Definitions serve to define unknowns (state variables), parameters
(constants during integration), and units of measurement.


``` 
    (define millivolt = unit Potential (1e-3 * volt)) 

    (define Vinit = parameter (dim Potential) -65.0 * millivolt) 

    (define v = unknown (dim Potential) -65.0 * mV) 
```

### Equations

Equations serve to define differential and algebraic equations.

```
     ((der(u)) = (s - u) / tau)
     ((s) = b * ((v - a) ^ 3))
```


### Events

```
     (event (v - Vthreshold)
            ((v := Vreset))
            )
```


## Examples

```scheme

;; Van der Pol oscillator
(define vdp 
  (parse 
   `(
     (define x = unknown -0.25)
     (define y = unknown 1.0)
     ((der(x)) = (1 - y ^ 2) * x - y )
     ((der(y)) = x)
     )
   ))


;; Izhikevich Fast Spiking neuron
(define izhfs 
  (parse 
   `(
     (define millivolt = unit Potential (1e-3 * volt))

     (define Isyn = parameter (dim Current) 0.0 * nA)
     (define Iext = parameter (dim Current) 400.0 * nA)

     (define k     = parameter 1.0)
     (define Vinit = parameter (dim Potential)  -65.0 * millivolt)
     (define Vpeak = parameter (dim Potential)   25.0 * mV)
     (define Vt    = parameter (dim Potential)  -55.0 * mV)
     (define Vr    = parameter (dim Potential)  -40.0 * mV)
     (define Vb    = parameter (dim Potential)  -55.0 * mV)
     (define Cm    = parameter (dim Capacitance) 20.0 * uF)

     (define FS_a = parameter  0.2)
     (define FS_b = parameter  (dim Current)   0.025 * nA)
     (define FS_c = parameter  (dim Potential) -45.0 * mV)
     (define FS_U = parameter  (dim Current) FS_b * (Vinit / mV))

     (define v  = unknown (dim Potential) -65.0 * mV)
     (define u  = unknown (dim Current) FS_U)
     (define s  = unknown (dim Current) 0.0 * nA)

     ((der(v)) = (((k * (v - Vr) * (v - Vt) / millivolt) + (((- u) + Iext) * megaohm)) / Cm) / megaohm)
     ((der(u)) = (FS_a * (s - u)) / ms)
     ((s) = FS_b * ((v - Vb) / mV) ^ 3)


     (event (v - Vpeak)
            ((v := FS_c)
             (u := u)
             (s := s)
             )
            )
     ))
  )


;; A routine to generate and compile Standard ML code 
(define (compile-model name model #!key (solver 'rk4b) (compile #f) (dir "tests"))
  (pp model)

  (define elab (elaborate model))
  (print "elaborate is done")
  (pp elab)

  (define sim (simcreate elab))
  (pp sim)
  (pp (codegen-ODE sim solver))
  (let* ((sml-path (make-pathname dir (string-append (->string name) ".sml")))
         (mlb-path (make-pathname dir (string-append (->string name) "_run.mlb")))
         (port (open-output-file sml-path)))
    (codegen-ODE/ML sim out: port solver: solver libs: '(interp))
    (close-output-port port)
    (if compile
        (run (mlton -mlb-path-var ,(sprintf "'SALT_HOME ~A'" SALT-DIR)
                    -mlb-path-var ,(sprintf "'RK_LIB $(SALT_HOME)/sml-lib/rk'")
                    -mlb-path-var ,(sprintf "'DYNAMICS_LIB $(SALT_HOME)/sml-lib/dynamics'")
                    ,mlb-path))))

)

(compile-model 'vdp vdp)
(compile-model 'izhfs izhfs)


```


## Version history

- 0.1 : Initial release

## License

>
> Copyright 2015 Ivan Raikov
> 
> This program is free software: you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation, either version 3 of the License, or (at
> your option) any later version.
> 
> This program is distributed in the hope that it will be useful, but
> WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
> General Public License for more details.
> 
> A full copy of the GPL license can be found at
> <http://www.gnu.org/licenses/>.
>

