import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub257 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case ((n, f), k) =>
      {
        k match {
          case 0 => { f(n) }
          case t => { iter(f(n), f, k - 1) }
        }
    }
  }
  
  /* TESTING FIELD BELOW */
  
}
