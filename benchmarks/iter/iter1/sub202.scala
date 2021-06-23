import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub202 {
  def iter[A](((n: Int63, f: A => A)), a: A): A = {
    if (n eq 0) a else iter(n - 1, f, f(a))
  }
}