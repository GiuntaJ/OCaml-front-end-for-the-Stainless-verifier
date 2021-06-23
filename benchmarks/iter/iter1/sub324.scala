import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub324 {
  /* 컴퓨터공학부 2013-11425 이창영 hw1_3 */
  
  def iter(((n: Int63, f)), x) = { if (n eq 0) x else iter(n - 1, f, f(x)) }
}