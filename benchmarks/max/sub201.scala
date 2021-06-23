import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub201 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { 0 }
          case Cons(hd, tl) => {
            val _2 = {
              def fold(f, l, a) = {
                l match {
                  case Nil() => { a }
                  case Cons(hd, tl) => { f(hd, fold(f, tl, a)) }
                }
              }
              val _3 = {
                def compare(x, y) = { if (x > y) x else y }
                fold(compare, tl, hd)
              }
            }
          }
        }
    }
  )
   
}