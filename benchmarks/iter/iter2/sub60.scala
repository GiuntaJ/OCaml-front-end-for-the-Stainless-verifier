import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub60 {
  /* problem 3*/
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def loop(n, x) = { if (n == 0) x else loop(n - 1, f(x)) }
          loop(n)
        }
    }
  }
}