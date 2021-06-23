import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub99 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def f1(x) = { if (n == 0) f(x - 2) else f(x + 2) }
          if (n == 0) f1 else iter(n - 1, f1)
        }
    }
  }
}
