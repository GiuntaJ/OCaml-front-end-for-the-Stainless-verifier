import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub6 {
  /* Problem 3 */
  def max(l: List[Int63]): Int63 = {
    l match {
      case Cons(n, Nil()) => { n + 0 }
      case Cons(h1, t1) => {
        t1 match {
          case Cons(h2, t2) => {
            if (h2 > h1) max(List(h2) ++ t2) else max(List(h1) ++ t2)
          }
        }
      }
    }
  }
}