import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub301 {
  def iter(((n: Int63, udf)), x) = { if (n <= 0) x else iter(n - 1, udf, udf(x))
  }
}