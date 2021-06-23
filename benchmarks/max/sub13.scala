import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub13 {
  def max(l: List[Int63]): Int63 = {
    l match {
      case Nil() => { -(1) }
      case Cons(a, Nil()) => { a }
      case Cons(h, t) => { if (h > max(t)) h else max(t) }
    }
  }
   
}