import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub132 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        val _4 = {
          val a = lst match {
            case Nil() => {
              assert(false, "Failure with list should not be empty")
            }
            case Cons(hd, tl) => { hd }
          }
          val _5 = {
            def fold(f, l, a) = {
              l match {
                case Nil() => { a }
                case Cons(hd, tl) => { f(hd, fold(f, tl, a)) }
              }
            }
            fold(
              {
                case (x, y) => { if (x > y) x else y }
              },
              lst, a)
          }
        }
    }
  )
   
}