import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub117 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case ((n, f), k) =>
      {
        (n, f) match {
          case (0, _) => { iter(1, ( (x) => { x } ), k) }
          case (1, _) => { f(k) }
          case (_, _) => { iter(n - 1, f, f(k)) }
        }
    }
  }
  
  iter(12, ( (x) => { x + 2 } ), 4)
}
