import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub173 {
  /********************/
  /*     Problem 1     */
  /********************/
  
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { assert(false, "Failure with The list is too short") }
          case Cons(a, Nil()) => { a }
          case Cons(a, Cons(b, tl)) => {
            if (a < b) max(b :: tl) else max(a :: tl)
          }
        }
    }
  ) 
   
}