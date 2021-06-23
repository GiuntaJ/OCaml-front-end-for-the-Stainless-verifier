import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub245 {
  def iter: ((Int63, (A => A)), A) => A = {
    case ((n, g), k) => { if (n > 0) g(iter(n - 1, g, k)) else k }
  }
}