import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub330 {
  /*자유전공학부 2013-13444 박하영*/
  
  def iter[A](((n: Int63, f: A => A)), x: A): A = {
    if (n eq 0) x else iter(n - 1, f, f(x))
  }
}
