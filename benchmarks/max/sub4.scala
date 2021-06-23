import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub4 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { 0 }
          case Cons(hd, tl) => {
            tl match {
              case Nil() => { hd + 0 }
              case Cons(hd1, tl1) => {
                if (hd > hd1) max(hd :: tl1) else max(hd1 :: tl1)
              }
            }
          }
        }
    }
  ) 
}