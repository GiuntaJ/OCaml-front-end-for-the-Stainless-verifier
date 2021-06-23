import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub216 {
  
  def iter(((n, f)), x) = { if (n > 0) iter(n - 1, f, f(x)) else x }
}