import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub71 {
  /*2009-11718 1-3*/
  
  def iter(((n, f)), x) = { if (n == 0) x else f(iter(n - 1, f, x)) }
}