import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub181 {
  /********************/
  /*     Problem 1     */
  /********************/
  def fold(f, lst) = {
    lst match {
      case Cons(a, Nil()) => { a }
      case Cons(hd, tl) => { f(hd, fold(f, tl)) }
    }
  }
  
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        fold(
          {
            case (x, y) => { if (x >= y) x else y }
          },
          lst)
    }
  )
   
}