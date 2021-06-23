import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub69 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { -(1) }
          case Cons(h, Nil()) => { h }
          case Cons(h, t) => {
            val _2 = {
              val temp = max(t)
              if (h > temp) h else temp
            }
          }
        }
    }
  )
   
}