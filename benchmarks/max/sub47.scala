import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub47 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { 0 }
          case Cons(h, Nil()) => { h }
          case Cons(h1, t1) => {
            t1 match {
              case Nil() => { h1 }
              case Cons(h, Nil()) => { if (h > h1) h else h1 }
              case Cons(h2, t2) => {
                if (h1 > h2) max(h1 :: t2) else max(h2 :: t2)
              }
            }
          }
        }
    }
  )
}