
(* 
 * randmtzig.sml
 *
 * MLton bindings for an implementation of the MT19937 random number
 * generator with Marsaglia and Tang's Ziggurat algorithm to generate
 * random numbers from a non-uniform distribution.
 * 
 * MLton code Copyright 2013 Ivan Raikov.
 * 
 * randmtzig.c Coded by Takuji Nishimura and Makoto Matsumoto.
 * This is a faster version by taking Shawn Cokus's optimization,
 * Matthe Bellew's simplification, Isaku Wada's real version.
 * David Bateman added normal and exponential distributions following
 * Marsaglia and Tang's Ziggurat algorithm.
 *
 *   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
 *   Copyright (C) 2004, David Bateman
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *   
 *     1. Redistributions of source code must retain the above copyright
 *        notice, this list of conditions and the following disclaimer;.
 *
 *     2. Redistributions in binary form must reproduce the above copyright
 *        notice, this list of conditions and the following disclaimer in the
 *        documentation and/or other materials provided with the distribution.
 *
 *     3. The names of its contributors may not be used to endorse or promote 
 *        products derived from this software without specific prior written 
 *        permission.
 *
 *
 *
 *)


signature RANDOM_MTZIG  =
sig

    type state
    type zt

    val ztnew: unit -> zt

    val fromInt: int -> state
    val fromArray: Word32Array.array -> state
    val fromEntropy: unit -> state

    val randUniform: state -> real
    val randNormal: state * zt -> real
    val randExp: state * zt -> real
    val randBinom: int * real * state -> real
    val randPoisson: real * state * zt -> real

    val randUniformArray: int * state -> Real64Array.array
    val randNormalArray: int * state * zt -> Real64Array.array
    val randExpArray: int * state * zt -> Real64Array.array
    val randBinomArray: int * real * int * state -> Real64Array.array
    val randPoissonArray: real * int * state * zt -> Real64Array.array
    val randNPoissonArray: (real -> real) * real * Real64Array.array * state * zt -> Real64Array.array

end


structure RandomMTZig: RANDOM_MTZIG =
struct

    type state = Word32Array.array 

    type zt = (Word64Array.array *
               Word64Array.array *
               Real64Array.array *
               Real64Array.array *
               Real64Array.array *
               Real64Array.array)

    val randmtzig_init_by_int = _import "randmtzig_init_by_int" : int * Word32Array.array -> unit;
    val randmtzig_init_by_array = _import "randmtzig_init_by_array" : Word32Array.array * Word32Array.array -> unit;
    val randmtzig_init_by_entropy = _import "randmtzig_init_by_entropy" :  Word32Array.array -> unit;

    (* === Uniform and non-uniform distribution generators === *)

    val randmtzig_randu = _import "randmtzig_randu" : Word32Array.array -> real;
    val randmtzig_randn = _import "randmtzig_randn" : Word32Array.array * Word64Array.array * Word64Array.array * 
                                                      Real64Array.array * Real64Array.array * 
                                                      Real64Array.array * Real64Array.array -> real;
    val randmtzig_rande = _import "randmtzig_rande" : Word32Array.array * Word64Array.array * Word64Array.array * 
                                                      Real64Array.array * Real64Array.array * 
                                                      Real64Array.array * Real64Array.array -> real;
    val randmtzig_randb = _import "randmtzig_randb" : int * real * Word32Array.array -> real;
    val randmtzig_randp = _import "randmtzig_randp" : real * Word32Array.array * Word64Array.array * Word64Array.array * 
                                                      Real64Array.array * Real64Array.array * 
                                                      Real64Array.array * Real64Array.array -> real;

    (* === Array generators === *)
    
    val randmtzig_fill_drandu = _import "randmtzig_fill_drandu" : int * Real64Array.array * Word32Array.array -> unit;

    val randmtzig_fill_drandn = _import "randmtzig_fill_drandn" : int * Real64Array.array * Word32Array.array * Word64Array.array * 
                                                                  Word64Array.array * Real64Array.array * Real64Array.array * 
                                                                  Real64Array.array * Real64Array.array -> unit;

    val randmtzig_fill_drande = _import "randmtzig_fill_drande" : int * Real64Array.array * Word32Array.array * Word64Array.array * 
                                                                  Word64Array.array * Real64Array.array * Real64Array.array * 
                                                                  Real64Array.array * Real64Array.array -> unit;

    val randmtzig_fill_drandb = _import "randmtzig_fill_drandb" : int * real * int * Real64Array.array * Word32Array.array -> unit;

    val randmtzig_fill_drandp = _import "randmtzig_fill_drandp" : real * int * Real64Array.array * Word32Array.array * Word64Array.array * 
                                                                  Word64Array.array * Real64Array.array * Real64Array.array * 
                                                                  Real64Array.array * Real64Array.array -> unit;

    val MT_N = 624
    val ZT_SIZE = 256

    fun ztnew () = (Word64Array.array (ZT_SIZE, Word64.fromInt 0),
                    Word64Array.array (ZT_SIZE, Word64.fromInt 0),
                    Real64Array.array (ZT_SIZE,  0.0),
                    Real64Array.array (ZT_SIZE,  0.0),
                    Real64Array.array (ZT_SIZE,  0.0),
                    Real64Array.array (ZT_SIZE,  0.0))

    fun fromInt (i) = 
        let 
            val st = Word32Array.array ((MT_N + 4), Word32.fromInt 0)
        in 
            randmtzig_init_by_int (i, st);
            st
        end

    fun fromArray (a) = 
        let 
            val st = Word32Array.array ((MT_N + 4), Word32.fromInt 0)
        in 
            randmtzig_init_by_array (a, st);
            st
        end

    fun fromEntropy () = 
        let 
            val st = Word32Array.array ((MT_N + 4), Word32.fromInt 0)
        in 
            randmtzig_init_by_entropy (st);
            st
        end

    fun randUniform (st) = randmtzig_randu st

    fun randBinom (n,p,st) = randmtzig_randb (n,p,st)

    fun randNormal (st,(ki,ke,wi,fi,we,fe)) = randmtzig_randn (st,ki,ke,wi,fi,we,fe)

    fun randExp (st,(ki,ke,wi,fi,we,fe)) = randmtzig_rande (st,ki,ke,wi,fi,we,fe)

    fun randPoisson (L,st,(ki,ke,wi,fi,we,fe)) = randmtzig_randp (L,st,ki,ke,wi,fi,we,fe)

    fun randUniformArray (n,st) = 
        let 
            val v = Real64Array.array (n,0.0)
        in 
            randmtzig_fill_drandu (n,v,st);
            v
        end

    fun randBinomArray (n,p,len,st) = 
        let 
            val v = Real64Array.array (len,0.0)
        in 
            randmtzig_fill_drandb (n,p,len,v,st);
            v
        end

    fun randNormalArray (n,st,(ki,ke,wi,fi,we,fe)) = 
        let 
            val v = Real64Array.array (n,0.0)
        in 
            randmtzig_fill_drandn (n,v,st,ki,ke,wi,fi,we,fe);
            v
        end


    fun randExpArray (n,st,(ki,ke,wi,fi,we,fe)) = 
        let 
            val v = Real64Array.array (n,0.0)
        in 
            randmtzig_fill_drande (n,v,st,ki,ke,wi,fi,we,fe);
            v
        end


    fun randPoissonArray (L,n,st,(ki,ke,wi,fi,we,fe)) = 
        let 
            val v = Real64Array.array (n,0.0)
        in 
            randmtzig_fill_drandp (L,n,v,st,ki,ke,wi,fi,we,fe);
            v
        end

(* Implementation of non-homogeneous Poisson process by thinning.

   Lewis, P. A. W.; Shedler, G. S. (1979). 
   "Simulation of nonhomogeneous poisson processes by thinning". 
   Naval Research Logistics Quarterly 26 (3): 403. 
*) 

(*
  For a given intensity function L (t):
  1. Define an upper bound Lmax for the intensity function L(t).
  2. Simulate a homogeneous Poisson process with intensity Lmax.
  3. "Thin" the simulated process as follows,
     (a) Compute p = L(t)/Lmax for each point (t) of the homogeneous Poisson process.
     (b) Generate a sample u from the uniform distribution on (0, 1).
     (c) Retain the locations for which u <= p.
*)

    fun randNPoissonArray (L, Lmax, t, st, zt) =
    let
        val sub = Real64Array.sub
        val n   = Real64Array.length t
        val hp  = randPoissonArray (Lmax, n, st, zt)
        val p   = Real64Array.tabulate (n, fn (i) => Real./(L (sub (t, i)), Lmax))
        val u   = randUniformArray (n, st)
    in
        Real64Array.fromList
            (Real64Array.foldri (fn (i,t,ax) => if Real.<= (sub (u,i), sub (p,i)) then t::ax else ax)
                                [] t)
    end


                                                   

end

fun uniformRandomArray seed size =
    let 
        val st   = RandomMTZig.fromInt seed
        val a    = Real64Array.array(size, RandomMTZig.randUniform st)
        fun loop 0 = a
          | loop j = (Real64Array.update(a, size-j, RandomMTZig.randUniform st);
                      loop (j-1))
    in loop (size - 1)
    end

fun normalRandomArray seed size =
    let 
        val st   = RandomMTZig.fromInt seed
        val zt   = RandomMTZig.ztnew()
        val a    = Real64Array.array(size, RandomMTZig.randUniform st)
        fun loop 0 = a
          | loop j = (Real64Array.update(a, size-j, RandomMTZig.randNormal (st,zt));
                      loop (j-1))
    in loop (size - 1)
    end

fun poissonRandomArray seed L size =
    let 
        val st   = RandomMTZig.fromInt seed
        val zt   = RandomMTZig.ztnew()
        val a    = Real64Array.array(size, RandomMTZig.randUniform st)
        fun loop 0 = a
          | loop j = (Real64Array.update(a, size-j, RandomMTZig.randPoisson (L,st,zt));
                      loop (j-1))
    in loop (size - 1)
    end


fun poissonNRandomArray seed L Lmax t =
    let 
        val st   = RandomMTZig.fromInt seed
        val zt   = RandomMTZig.ztnew()
    in 
        RandomMTZig.randNPoissonArray (L, Lmax, t, st, zt) 
    end



fun realArrayWrite file x =
    (Real64Array.app (fn x => (TextIO.output(file, " " ^ (Real.toString x)))) x)
(*
val _ = (let open RandomMTZig
             val n = 10
	     val uv = uniformRandomArray 19 n
	     val nv = normalRandomArray 19 n
	     val pv = poissonRandomArray 19 0.5 n
	 in
             (realArrayWrite (TextIO.stdOut) uv; TextIO.output (TextIO.stdOut,"\n");
              realArrayWrite (TextIO.stdOut) nv; TextIO.output (TextIO.stdOut,"\n");
              realArrayWrite (TextIO.stdOut) pv; TextIO.output (TextIO.stdOut,"\n"))
	 end)
*)
(*
val _ = (let open RandomMTZig
             fun L (x) = if Real.< (x,1000.0) then 100.0 else 200.0
             val t = Real64Array.tabulate (2000, fn (i) => Real.* (1.0, Real.fromInt i))
	     val pv = poissonNRandomArray 17 L 200.0 t
	 in
             realArrayWrite (TextIO.stdOut) pv; TextIO.output (TextIO.stdOut,"\n")
	 end)
*)
