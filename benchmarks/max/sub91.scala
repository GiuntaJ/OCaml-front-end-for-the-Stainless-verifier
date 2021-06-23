import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub91 {
  /********************/
  /*     Problem 1     */
  /********************/
  def big[A](a: A, b: A): A = { if (a > b) a else b }
  
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { -(100000) }
          case Cons(hd, tl) => { big(hd, max(tl)) }
        }
    }
  )
   
}