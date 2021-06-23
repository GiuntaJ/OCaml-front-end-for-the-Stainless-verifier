import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub12 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { 0 }
          case Cons(x, Nil()) => { x }
          case Cons(x, xm) => {
            val _2 = {
              val v = max(xm)
              if (x < v) v else x
            }
          }
        }
    }
  )
   
}