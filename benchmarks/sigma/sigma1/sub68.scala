import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub68 {
  /* PL HW1-1 "씨그마"
     2007-11738
     알렉산더 */
  
  /* sigma: int * int * (int->int) -> int */
  def sigma(((a, b, func))) = {
    if (a > b) 0 else func(a) + sigma(a + 1, b, func)
  }
}