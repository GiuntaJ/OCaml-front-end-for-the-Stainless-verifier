import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub114 {
  def iter: ((Int63, (A => A)), A) => A = {
    case (n, f) =>
      {
        val _2 = {
          val _f: A => A = ( (_a) => { f(iter(n - 1, f, _a)) } )
          
            if (
              n > 0
            ) {
              _f 
            } else {
              val _4 = {
                val _f: A => A = ( (_a) => { _a } )
                _f
              }
            }
        }
    }
  } 
  /*
  let _ =
  	let f : int -> int = fun(n) -> n+1 in
  	let f2 : int -> int = fun(m) -> 2*m in
  	print_int(iter(3,f)(1));
  	print_int(iter(10,f2)(1));
  */
}