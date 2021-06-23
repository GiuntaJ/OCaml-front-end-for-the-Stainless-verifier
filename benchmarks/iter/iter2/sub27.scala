import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub27 {
    /*Problem 3*/
  def iter(((n, f: Int63 => Int63))) = {
    n match {
      case 0 => { ( (x) => { x } ) }
      case 1 => { f }
      case _ => { ( (x) => { f(iter(n - 1, f, x)) } ) }
    }
  }
}