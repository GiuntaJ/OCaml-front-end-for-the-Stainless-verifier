import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub8 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { min_int }
          case Cons(h, t) => { if (max(t) > h) max(t) else h }
        }
    }
  )
}