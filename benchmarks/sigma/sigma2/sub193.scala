import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub193 {
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) =>
      {
        
          if (
            a > b
          ) {
            sigma(f, b, a) 
          } else {
            val _3 = {
              def loop(a, r) = { if (a == b) r + f(a) else loop(a + 1, r + f(a))
              }
              loop(a, 0)
            }
          }
    }
  }
}