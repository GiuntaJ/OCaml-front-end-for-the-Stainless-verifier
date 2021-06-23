import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub221 {
  def iter[A](((n: Int63, f: A => A)), x: A): A = {
    if (n <= 0) x else iter(n - 1, f, f(x))
  }
}