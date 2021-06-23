import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub44 {
  def iter(((n, f))) = {
    n match {
      case 0 => { ( (a) => { a } ) }
      case _ => { ( (a) => { iter(n - 1, f, f(a)) } ) }
    }
  }
}