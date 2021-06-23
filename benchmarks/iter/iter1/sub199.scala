import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub199 {
  def iter: ((Int63, (A => A)), A) => A = {
    case (n, fn) =>
      {
        val _2 = {
          val nest: (((A => A), (A => A)), A) => A = {
            case ((fn1, fn2), x) => { fn1(fn2(x)) }
          }
          if (n <= 0) ( (x) => { x } ) else nest(fn, iter(n - 1, fn))
        }
    }
  }
}