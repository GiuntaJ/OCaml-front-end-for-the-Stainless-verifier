import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub122 {
  def max[A](l: List[A]): A = {
    l match {
      case Nil() => { assert(false, "Failure with list is too short..") }
      case Cons(hd, tl) => {
        
          if (
            tl == Nil()
          ) {
            hd 
          } else if (
            hd >= max(tl)
          ) {
            hd 
          } else {
            max(tl)
          }
      }
    }
  }
   
}