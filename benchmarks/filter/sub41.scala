import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_filter_sub41 {
  def filter(p) = {
    (
      x =>
        x match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => {
            if (p(hd)) hd :: filter(p, tl) else filter(p, tl)
          }
        }
    )
  }
}