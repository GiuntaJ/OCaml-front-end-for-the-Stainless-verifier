import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub15 {
  /* School of Computer Science & Engineering
   * 2009-23151
   * 조성근
   * HW 1 - Exercise 2
   */
  
  def iter(((n, f)), a) = { if (n == 0) a else f(iter(n - 1, f, a)) }
}