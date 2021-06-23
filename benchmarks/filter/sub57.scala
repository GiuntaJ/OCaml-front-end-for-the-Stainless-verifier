import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_filter_sub57 {
  /********************/
  /* Problem 1: filter */
  /********************/
  
  def filter: (A => Boolean, List[A]) => List[A] = {
    case (pred, lst) =>
      {
        lst match {
          case Nil() => { Nil() }
          case Cons(x, tail) => {
            if (pred(x)) x :: filter(pred, tail) else filter(pred, tail)
          }
        }
    }
  }
}