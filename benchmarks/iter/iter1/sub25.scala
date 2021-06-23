import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub25 {
  def iter: ((Int63, (A => A)), A) => A = {
    case ((n, f), x) =>
      {
        
          if (
            n > 0
          ) {
            iter(n - 1, f, f(x)) 
          } else if (
            n == 0
          ) {
            x 
          } else {
            assert(false, "Invalid_argument with n must be >=0")
          }
    }
  }
}