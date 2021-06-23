import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_filter_sub81 {
  def filter: (A => Boolean, List[A]) => List[A] = {
    case (f, l) =>
      {
        l match {
          case Nil() => { Nil() }
          case Cons(h, t) => { if (f(h)) h :: filter(f, t) else filter(f, t) }
        }
    }
  }
}