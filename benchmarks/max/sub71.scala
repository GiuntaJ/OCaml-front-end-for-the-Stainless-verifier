import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub71 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { assert(false, "Failure with list is too short") }
          case Cons(x, Nil()) => { x }
          case Cons(hd, tl) => { if (hd > max(tl)) hd else max(tl) }
        }
    }
  ) 
   
}