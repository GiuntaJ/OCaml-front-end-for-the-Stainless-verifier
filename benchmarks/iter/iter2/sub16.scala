import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub16 {
  /* Problem 3 */
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n > 0
          ) {
            (
              (x) =>
                {
                  val _7 = {
                    val f2 = iter(n - 1, f)
                    f2(f(x))
                  }
              }
            ) 
          } else {
            ( (x) => { x } )
          }
    }
  }
}