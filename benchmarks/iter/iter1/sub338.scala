import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub338 {
  
  def iter(((n: Int63, f: Int63 => Int63)), x) = {
    if (n eq 0) 0 else f(iter(n - 1, f, x))
  }
}