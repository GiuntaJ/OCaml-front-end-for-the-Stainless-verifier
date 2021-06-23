import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub21 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { assert(false, "Failure with Empty List") }
          case Cons(hd, Nil()) => { hd }
          case Cons(hd, tl) => {
            val _2 = {
              val m = max(tl)
              if (hd > m) hd else m
            }
          }
        }
    }
  )
}