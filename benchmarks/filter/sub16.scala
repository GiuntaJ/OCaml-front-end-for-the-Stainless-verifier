import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_filter_sub16 {
  /********************/
  /* Problem 1: filter */
  /********************/
  def filter(pred, lst) = {
    lst match {
      case Nil() => { Nil() }
      case Cons(head, tail) => {
        if (pred(head)) head :: filter(pred, tail) else filter(pred, tail)
      }
    }
  }
}