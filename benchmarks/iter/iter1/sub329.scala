import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub329 {
  def iter[A](((n: Int63, f: A => A)), param: A): A = {
    (n, f) match {
      case (0, f) => { param }
      case (n, f) => { f(iter(n - 1, f, param)) }
    }
  }
}