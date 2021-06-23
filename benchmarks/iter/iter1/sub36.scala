import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub36 {
  /* 컴퓨터공학부/2009-11679/김정명/2 */
  
  def iter(((n, f)), a) = { if (n == 0) a else iter(n - 1, f, f(a)) }
}