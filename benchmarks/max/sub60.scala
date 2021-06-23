import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub60 {
  def max[A](p: List[A]): A = {
    p match {
      case Cons(t, Nil()) => { t }
      case Cons(hd, tl) => { if (hd > max(tl)) hd else max(tl) }
    }
  }
   
}