import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_filter_sub194 {
  def filter: (A => Boolean, List[A]) => List[A] = {
    case (pred, lst) =>
      {
        val _2 = {
          def rfilter: List[A] => List[A] = (
            (l) =>
              {
                l match {
                  case Nil() => { Nil() }
                  case Cons(h, t) => {
                    if (pred(h) == true) h :: rfilter(t) else rfilter(t)
                  }
                }
            }
          )
          rfilter(lst)
        }
    }
  }
}