import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub315 {
  def iter: ((Int63, (A => A)), A) => A = {
    case ((n, f), i) => { if (n eq 0) i else iter(n - 1, f, f(i)) }
  }
}