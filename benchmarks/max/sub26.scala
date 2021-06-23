import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub26 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Cons(n, Nil()) => { n }
          case Cons(hd, tl) => { if (hd > max(tl)) hd else max(tl) }
        }
    }
  )
   
}