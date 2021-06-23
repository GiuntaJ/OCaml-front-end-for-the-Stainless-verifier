import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub9 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Cons(hd, Nil()) => { hd }
          case Cons(hd, tl) => { if (hd > max(tl)) hd else max(tl) }
        }
    }
  )
}