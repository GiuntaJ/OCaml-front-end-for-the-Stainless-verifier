import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub172 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        val _4 = {
          def fold(action, l, a) = {
            l match {
              case Nil() => { a }
              case Cons(hd, tl) => { action(hd, fold(action, tl, a)) }
            }
          }
          val _5 = {
            def max(a, b) = { if (a > b) a else b }
            fold(max, lst, 0)
          }
        }
    }
  )
   
}