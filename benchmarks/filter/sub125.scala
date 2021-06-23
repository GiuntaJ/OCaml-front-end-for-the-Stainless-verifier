import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_filter_sub125 {
  def filter: (A => Boolean, List[A]) => List[A] = {
    case (pred, lst) => { Nil() }
  }
}