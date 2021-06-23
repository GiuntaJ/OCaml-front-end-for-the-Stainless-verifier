import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub144 {
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) =>
      {
        val _2 = {
          def sum(now_sum, f, now_i, end_i) = {
            now_i > end_i match {
              case true => { now_sum }
              case false => { sum(now_sum + f(now_i), f, now_i + 1, end_i) }
            }
          }
          sum(0, f, a, b)
        }
    }
  }
}