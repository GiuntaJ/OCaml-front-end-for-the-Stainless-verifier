import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub193 {
  def iter[A](((n: Int63, f: A => A)), a: A): A = {
    val _2 = {
      val b = f(a)
      if (n <= 0) a else iter(n - 1, f, b)
    }
  }
}