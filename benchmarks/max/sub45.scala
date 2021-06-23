import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub45 {
  /* Problem 3 */
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { assert(false, "Failure with List is Empty") }
          case Cons(hd, Nil()) => { hd }
          case Cons(hd, tl) => { if (hd > max(tl)) hd else max(tl) }
        }
    }
  )
   
}