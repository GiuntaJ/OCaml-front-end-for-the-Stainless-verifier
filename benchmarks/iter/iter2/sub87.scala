import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub87 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) => { if (n == 0) f else iter(n - 1, f) }
  }
        
  iter(4, ( (x) => { 6 + x } ), 0)
}