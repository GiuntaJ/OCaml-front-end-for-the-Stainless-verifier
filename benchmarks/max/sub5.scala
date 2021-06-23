import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub5 {
  def max: List[Int63] => Int63 = ( (l) => { 1 } )
  def max[A](l: List[A]): A = {
    l match {
      case Nil() => { assert(false, "Failure with Too short") }
      case Cons(hd, tl) => {
        
          if (
            tl == Nil()
          ) {
            hd 
          } else {
            val _3 = {
              val r = max(tl)
              if (hd <= r) r else hd
            }
          }
      }
    }
  }
}