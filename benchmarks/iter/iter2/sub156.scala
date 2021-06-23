import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub156 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def a(f, b, x) = { f(b(x)) }
          if (n == 0) f else a(f, iter(n - 1, f))
        }
    }
  }
    iter(2, ( (x) => { 2 + x } ), 0)
}