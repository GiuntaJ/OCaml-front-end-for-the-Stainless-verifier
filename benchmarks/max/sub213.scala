import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub213 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        val _4 = {
          def maxx: (Int63, Int63) => Int63 = {
            case (a, b) => { if (a > b) a else b }
          }
          lst match {
            case Cons(m, Nil()) => { m }
            case Cons(m, n) => { maxx(m, max(n)) }
          }
        }
    }
  )
   
}