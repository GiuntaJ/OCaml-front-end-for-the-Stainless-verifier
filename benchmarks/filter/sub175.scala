import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_filter_sub175 {
  def filter(pred, lst) = {
    List.fold_right(
      {
        case (x, a) => { if (pred(x)) x :: a else a }
      },
      lst, Nil())
  }
}