import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub20 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { 0 }
          case Cons(hd, Nil()) => { hd }
          case Cons(hd, tl) => {
            val _2 = {
              val r = max(tl)
              if (hd > r) hd else r
            }
          }
        }
    }
  )
   
}