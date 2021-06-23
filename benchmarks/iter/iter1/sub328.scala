import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub328 {
  val iter: ((Int63, (A => A)), A) => A = {
    case (n, f) =>
      {
        val _2 = {
          def _iter[A](_n: Int63, acc: A => A) = {
            if (_n <= 0) acc else _iter(_n - 1, ( (x) => { acc(f(x)) } ))
          }
          _iter(n, ( (x) => { x } ))
        }
    }
  }
}