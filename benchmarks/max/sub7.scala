import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub7 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { assert(false, "Failure with Emplty list!") }
          case Cons(x, Nil()) => { x }
          case Cons(x, y) => {
            val _2 = {
              val temp = max(y)
              if (x > temp) x else temp
            }
          }
        }
    }
  )
}