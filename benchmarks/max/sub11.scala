import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub11 {
  def max[A](l: List[A]): A = {
    l match {
      case Cons(x, Nil()) => { x }
      case Cons(hd, tl) => { if (hd > max(tl)) hd else max(tl) }
    }
  }
   
}