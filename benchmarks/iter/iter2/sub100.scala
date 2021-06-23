import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub100 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        (n, f) match {
          case (0, _) => { ( (a) => { a } ) }
          case (_, _) => { ( (a) => { iter(n - 1, f, f(a)) } ) }
        }
    }
  }
      
}