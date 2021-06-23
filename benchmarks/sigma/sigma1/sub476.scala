import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub476 {
  /*
   * Dept of Physics Education
   * 2012-12666 Choi Jaehyeok
   * Homework 1, Problem 2
   */
  
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) => { if (a > b) 0 else f(a) + sigma(a + 1, b, f) }
  }
}