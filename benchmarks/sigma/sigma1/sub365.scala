import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub365 {
  val sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) =>
      {
        val _2 = {
          def aux(((a, b, f, result))) = {
            if (a > b) result else aux(a + 1, b, f, result + f(a))
          }
          aux(a, b, f, 0)
        }
    }
  }
}