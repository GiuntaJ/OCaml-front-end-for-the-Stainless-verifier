import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub287 {
  def iter: ((Int63, (A => A)), A) => A = {
    case (n, f) =>
      { if (n > 0) ( (x) => { iter(n - 1, f, f(x)) } ) else ( (x) => { x } )
    }
  }
  
  /*
  let a31 = iter (3, function x -> 2+x) 0 
  let a32 = iter (0, function x -> 2*x) 4 
  let a33 = iter (11, function x -> 2*x+1) 7
  
  let _ = print_int a31 ; print_int a32 ; print_int a33
  */
}