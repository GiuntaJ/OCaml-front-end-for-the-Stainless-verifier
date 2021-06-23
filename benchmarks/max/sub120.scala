import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub120 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        val _4 = {
          val h = lst match {
            case Nil() => { 0 }
            case Cons(hd, tl) => { hd }
          }
          val _5 = {
            def fold(f, l, b) = {
              l match {
                case Nil() => { h }
                case Cons(hd, tl) => { f(hd, fold(f, tl, h)) }
              }
            }
            fold(
              {
                case (x, y) => { if (x > y) x else y }
              },
              lst, h)
          }
        }
    }
  )
   
}