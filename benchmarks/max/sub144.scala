import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub144 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max1[A](a: A, b: A): A = { if (a >= b) a else b } 
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Cons(hd, Nil()) => { hd }
          case Cons(hd, tl) => { max1(hd, max(tl)) }
        }
    }
  )
   
}