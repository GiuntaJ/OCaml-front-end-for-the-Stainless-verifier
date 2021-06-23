import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub86 {
  def iter(((a, b))) = {
    (a, b) match {
      case (0, _) => { ( (x) => { x } ) }
      case _ => { ( (x) => { iter(a - 1, b, b(x)) } ) }
    }
  }
}