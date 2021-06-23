import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub83 {
  /********************/
  /*     Problem 1     */
  /********************/
  def num_max[A](a: A, b: A): A = { if (a > b) a else b }
  
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { assert(false, "Failure with list is too short") }
          case Cons(x, Nil()) => { x }
          case Cons(x, tl) => { num_max(x, max(tl)) }
        }
    }
  )
   
}