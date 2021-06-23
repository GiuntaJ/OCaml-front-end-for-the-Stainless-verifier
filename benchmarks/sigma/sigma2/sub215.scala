import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub215 {
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) =>
      {
        val _2 = {
          def cal(sum, f, now_i, end_i) = {
            if (now_i > end_i) sum else cal(sum + f(now_i), f, now_i + 1, end_i)
          }
          cal(0, f, a, b)
        }
    }
  }
}