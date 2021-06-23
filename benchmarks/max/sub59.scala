import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub59 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { 0 }
          case Cons(h, Nil()) => { h }
          case Cons(h, t) => { if (max(t) < h) h else max(t) }
        }
    }
  )
   
}