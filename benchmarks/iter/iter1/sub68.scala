import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub68 {
  /* HW1 exercise3 2009-11697 Kim HyunJoon*/
  /* Iter */
  
  def iter: ((Int63, (A => A)), A) => A = {
    case ((n, f), x) => { if (n <= 0) x else iter(n - 1, f, f(x)) }
  }
}