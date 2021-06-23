import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub63 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { 0 }
          case Cons(d, Nil()) => { d }
          case Cons(d, e) => {
            val _2 = {
              val f = max(e)
              if (d > f) d else f
            }
          }
        }
    }
  )
   
}