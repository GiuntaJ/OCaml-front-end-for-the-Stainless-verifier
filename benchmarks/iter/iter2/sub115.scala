import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub115 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) => { if (n == 0) f else ( (x) => { iter(n - 1, f, f(x)) } ) }
  }
  iter(2, ( (x) => { 2 + x } ), 0)
}