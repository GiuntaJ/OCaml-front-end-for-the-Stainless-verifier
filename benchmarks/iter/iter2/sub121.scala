import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub121 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n == 0
          ) {
            ( (x: Int63) => { x } ) 
          } else {
            ( (x: Int63) => { f(iter(n - 1, f, x)) } )
          }
    }
  }
  
  /*iter(2, fun x -> 2+x) 0;;*/
}
