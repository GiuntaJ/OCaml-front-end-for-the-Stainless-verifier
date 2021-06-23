import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub49 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Cons(a, Nil()) => { a }
          case Cons(h, t) => {
            val _2 = {
              val m = max(t)
              if (h > m) h else m
            }
          }
        }
    }
  )
   
}