import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub28 {
  /* PL HW1-2 "반복기"
     2007-11738
     알렉산더 */
  
  /* iter: int * (int -> int) -> int -> int */
  def iter(((n, f)), x) = { if (n == 0) x else f(iter(n - 1, f, x)) }
}