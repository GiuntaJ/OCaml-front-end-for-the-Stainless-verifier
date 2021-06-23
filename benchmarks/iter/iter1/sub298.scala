import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub298 {
  /*
   * Dept of Physics Education
   * 2012-12666 Choi Jaehyeok
   * Homework 1, Problem 3
   */
  
  def iter: ((Int63, (A => A)), A) => A = {
    case ((n, f), v) => { if (n <= 0) v else iter(n - 1, f, f(v)) }
  }
}