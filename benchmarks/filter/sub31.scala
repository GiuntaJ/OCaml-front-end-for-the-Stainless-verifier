import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_filter_sub31 {
  /********************/
  /* Problem 1: filter */
  /********************/
  def filter(pred, lst) = {
    lst match {
      case Nil() => { Nil() }
      case Cons(hd, rest) => {
        if (pred(hd)) hd :: filter(pred, rest) else filter(pred, rest)
      }
    }
  }
}