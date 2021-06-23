import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub18 {
  /* Problem 3 */
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { 0 }
          case Cons(x, Nil()) => { x }
          case Cons(x, tail) => {
            val _2 = {
              val v = max(tail)
              if (x > v) x else v
            }
          }
        }
    }
  )
   
}