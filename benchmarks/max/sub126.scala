import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub126 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lsh) =>
      {
        val _4 = {
          def maxh: (Int63, Int63) => Int63 = {
            case (a, b) => { if (a > b) a else b }
          }
          lsh match {
            case Cons(m, Nil()) => { m }
            case Cons(m, n) => { maxh(m, max(n)) }
          }
        }
    }
  )
  
   
}