import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub41 {
  /* Problem 3 */
  def max[A](l: List[A]): A = {
    l match {
      case Nil() => { assert(false, "Failure with Empty List") }
      case Cons(hd, Nil()) => { hd }
      case Cons(hd, tl) => { if (hd > max(tl)) hd else max(tl) }
    }
  }
   
}