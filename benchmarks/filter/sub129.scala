import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_filter_sub129 {
  def filter(pred, lst) = {
    lst match {
      case Nil() => { assert(false, "Failure with NO item") }
      case Cons(a, Nil()) => { if (pred(a)) List(a) else Nil() }
      case Cons(hd, tl) => {
        if (pred(hd)) hd :: filter(pred, tl) else filter(pred, tl)
      }
    }
  }
}