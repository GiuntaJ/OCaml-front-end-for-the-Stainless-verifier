import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub311 {
  def iter: ((Int63, (A => A)), A) => A = {
    case ((n, f), a) => { if (n <= 0) a else iter(n - 1, f, f(a)) }
  }
}