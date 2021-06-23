import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub136 {
  val iter: ((Int63, (A => A)), A) => A = {
    case ((n, f), y) =>
      {
        val _2 = {
          def recursion(((k, x))) = { if (k < 1) x else recursion(k - 1, f(x)) }
          recursion(n, y)
        }
    }
  }
}