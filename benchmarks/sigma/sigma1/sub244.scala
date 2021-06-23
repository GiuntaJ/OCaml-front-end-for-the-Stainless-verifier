import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub244 {
  /* College of Liberal Studies 2010-13342 Kim Ye Jung */
  /* 2014.2 Programming Languages Homework 1 - 1 */
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) => { if (a > b) 0 else sigma(a + 1, b, f) + f(a) }
  }
}