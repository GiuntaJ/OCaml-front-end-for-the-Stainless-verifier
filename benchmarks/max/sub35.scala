import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub35 {
  sealed case class Empty_list() extends Exception {}
  
  def max(l: List[Int63]): Int63 = {
    l match {
      case Nil() => { assert(false, "Empty_list") }
      case Cons(h, Nil()) => { 0 + h }
      case Cons(h, t) => { if (h > max(t)) h else max(t) }
    }
  }
   
}