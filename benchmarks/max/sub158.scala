import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub158 {
  def max(lst: List[Int63]): Int63 = {
    lst match {
      case Nil() => { 0 }
      case Cons(x, Nil()) => { x }
      case Cons(x, xs) => {
        val _2 = {
          val v = max(xs)
          if (x < v) v else x
        }
      }
    }
  }
   
}