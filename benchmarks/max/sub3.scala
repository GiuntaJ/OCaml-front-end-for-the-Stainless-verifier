import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub3 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { min_int }
          case Cons(h, t) => { if (h > max(t)) h else max(t) }
        }
    }
  )
}