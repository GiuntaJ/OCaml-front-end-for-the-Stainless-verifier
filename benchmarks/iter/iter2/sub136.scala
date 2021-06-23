import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub136 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        n match {
          case 0 => { ( (x) => { x } ) }
          case n => { ( (x) => { iter(n - 1, f, f(x)) } ) }
        }
    }
  }
      
  iter(3, ( (x) => { x + 1 } ), 3)
}