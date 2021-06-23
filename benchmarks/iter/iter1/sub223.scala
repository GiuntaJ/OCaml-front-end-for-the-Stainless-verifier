import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub223 {
  def iter[A](((n: Int63, f: A => A)), x: A): A = {
    (n, f) match {
      case (0, _) => { x }
      case (_, _) => { f(iter(n - 1, f, x)) }
    }
  }
}