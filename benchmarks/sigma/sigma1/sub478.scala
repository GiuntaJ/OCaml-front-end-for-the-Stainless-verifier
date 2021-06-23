import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub478 {
  def sigma(((x: Int63, y: Int63, udf))): Int63 = {
    if (x > y) 0 else udf(x) + sigma(x + 1, y, udf)
  }
}