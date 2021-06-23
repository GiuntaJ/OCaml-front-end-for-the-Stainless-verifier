import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub165 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        val _4 = {
          val retnmax: (Int63, Int63) => Int63 = {
            case (a, b) => { if (a > b) a else b }
          }
          lst match {
            case Cons(a, Nil()) => { a }
            case Cons(hd, tl) => { retnmax(hd, max(tl)) }
          }
        }
    }
  )
   
}