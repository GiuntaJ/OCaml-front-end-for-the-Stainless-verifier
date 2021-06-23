import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_filter_sub97 {
  def filter: (A => Boolean, List[A]) => List[A] = {
    case (pred, lst) =>
      {
        lst match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => {
            if (pred(hd)) List(hd) ++ filter(pred, tl) else filter(pred, tl)
          }
        }
    }
  }
}