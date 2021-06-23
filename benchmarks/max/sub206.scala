import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub206 {
  def fold(f, lst, base) = {
    lst match {
      case Nil() => { base }
      case Cons(i, Nil()) => { i }
      case Cons(hd, tl) => { f(hd, fold(f, tl, base)) }
    }
  }
  
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        fold(
          {
            case (a, b) => { if (a > b) a else b }
          },
          lst, 0)
    }
  )
   
}