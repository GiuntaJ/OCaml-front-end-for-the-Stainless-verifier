import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub155 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case ((n, f), b) => { if (n == 0) b else iter(n - 1, f, f(b)) }
  } 
}
