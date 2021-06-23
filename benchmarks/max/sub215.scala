import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub215 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        val _4 = {
          def bigger(a, b) = { if (a >= b) a else b }
          lst match {
            case Nil() => { 0 }
            case Cons(hd, tl) => { List.fold_left(bigger, hd, tl) }
          }
        }
    }
  )
   
}