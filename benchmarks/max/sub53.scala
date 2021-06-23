import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub53 {
  def max(l: List[Int63]): Int63 = {
    l match {
      case Nil() => { 0 }
      case Cons(a, Nil()) => { a }
      case Cons(a, b) => {
        val _2 = {
          val c = max(b)
          if (a > c) a else c
        }
      }
    }
  }
   
}