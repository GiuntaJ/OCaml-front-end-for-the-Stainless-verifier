import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub34 {
   
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { assert(false, "Failure with error") }
          case Cons(a, Nil()) => { a }
          case Cons(a, Cons(b, tl)) => {
            if (a > b) max(a :: tl) else max(b :: tl)
          }
        }
    }
  )
}