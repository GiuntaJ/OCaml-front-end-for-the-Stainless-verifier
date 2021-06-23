import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub207 {
  /********************/
  /*     Problem 1     */
  /********************/
  
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        val _4 = {
          def fold(f, l) = {
            l match {
              case Cons(a, Nil()) => { a }
              case Cons(hd, tl) => { f(hd, fold(f, tl)) }
            }
          }
          fold(
            {
              case (x, y) => { if (x >= y) x else y }
            },
            lst)
        }
    }
  )
   
}