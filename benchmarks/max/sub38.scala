import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub38 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { 0 }
          case Cons(last, Nil()) => { last }
          case Cons(hd, tl) => {
            val _2 = {
              val m = max(tl)
              if (hd >= m) hd else m
            }
          }
        }
    }
  ) 
   
}