signature MODEL =
sig
    
    val n: int
                
    val nev: int
                  
    val nregime: int
                      
    val ndsc: int

    val paramfun: unit -> real array
    val stepfun: {ext: real array, extev: real array, fld: real array, p: real array} *
	         real * real * real array * real array -> real array * real array * real array FunQueue.t
end
