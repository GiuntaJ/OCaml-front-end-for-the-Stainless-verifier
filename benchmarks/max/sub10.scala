import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub10 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { assert(false, "Failure with List is too short!") }
          case Cons(a, Nil()) => { a }
          case Cons(hd, tl) => { if (hd < max(tl)) max(tl) else hd }
        }
    }
  )
   
}