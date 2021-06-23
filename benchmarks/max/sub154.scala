import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub154 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Cons(hd, Nil()) => { hd }
          case Cons(hd, tl) => { big(hd, max(tl)) }
        }
    }
  )
  def big(a: Int63, b: Int63): Int63 = { if (a < b) b else a }
   
}