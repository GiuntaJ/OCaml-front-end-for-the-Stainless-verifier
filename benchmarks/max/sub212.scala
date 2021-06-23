import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub212 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { 0 }
          case Cons(h, Nil()) => { h }
          case Cons(h, t) => {
            val _2 = {
              val x = max(t)
              if (h > x) h else x
            }
          }
        }
    }
  )
   
}