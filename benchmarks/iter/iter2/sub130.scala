import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub130 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        n match {
          case 0 => { ( (x) => { x } ) }
          case _ => { ( (x) => { f(iter(n - 1, f, x)) } ) }
        }
    }
  }
    
  iter(3, ( (x) => { 2 + x } ), 0)
}