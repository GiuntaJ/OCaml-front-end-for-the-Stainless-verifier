import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub159 {
  /********************/
  /*     Problem 1     */
  /********************/
  
  def max(lst: List[Int63]): Int63 = {
    val _2 = {
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
        lst, 0)
    }
  }
   
}