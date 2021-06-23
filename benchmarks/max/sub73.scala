import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub73 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { assert(false, "Failure with Empty list") }
          case Cons(a, Nil()) => { a }
          case Cons(hd, tl) => {
            val _2 = {
              val maxintl = max(tl)
              if (maxintl < hd) hd else maxintl
            }
          }
        }
    }
  )
   
}