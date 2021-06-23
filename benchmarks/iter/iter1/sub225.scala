import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub225 {
  /* 2015 - 14718 Giyeon Kim HW 1 */
  
  /* Exercise 3 */
  def iter(((n, f)), x) = { if (n <= 0) x else iter(n - 1, f, f(x)) }
}
