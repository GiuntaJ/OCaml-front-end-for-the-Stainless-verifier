import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub78 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Cons(e, Nil()) => { e }
          case Cons(h, t) => {
            val _2 = {
              val v = max(t)
              if (h >= v) h else v
            }
          }
        }
    }
  )
}