import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub52 {
  sealed case class Not_found() extends Exception {}
  
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Cons(a, Nil()) => { a }
          case Cons(hd, tl) => { if (hd > max(tl)) hd else max(tl) }
          case Nil() => { assert(false, "Not_found") }
        }
    }
  )
   
}