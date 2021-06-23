import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub96 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max2: (Int63, Int63) => Int63 = {
    case (a, b) => { if (a >= b) a else b }
  }
   
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { -(999999999) }
          case Cons(hd, tl) => { max2(hd, max(tl)) }
        }
    }
  )
   
}