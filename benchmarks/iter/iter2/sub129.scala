import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub129 {
  def realIter(((n, f, f_original))) = {
    n match {
      case 0 => { ( (x) => { x } ) }
      case 1 => { f }
      case _ => { realIter(n - 1, ( (x) => { f(f_original(x)) } ), f_original) }
    }
  }
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) => { realIter(n, f, f) }
  }
}