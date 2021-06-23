import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub167 {
  /* sigma(a,b,f) := SIGMA i = a->b with function f(i) */
  sealed case class INVALID_RANGE() extends Exception {}
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = val _0 = {
    def sigma_rec: (Int63, Int63, Int63, (Int63 => Int63)) => Int63 = {
      case (sum, i, b, f) =>
        {
          
            if (
              i < b
            ) {
              sigma_rec(sum + f(i), i + 1, b, f) 
            } else if (
              i == b
            ) {
              sum + f(i) 
            } else {
              assert(false, "INVALID_RANGE")
            }
      }
    }
    {
      case (a, b, f) => { if (a > b) 0 else sigma_rec(0, a, b, f) }
    }
  }
}
