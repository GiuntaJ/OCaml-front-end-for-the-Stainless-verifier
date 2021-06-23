import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub211 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max(l: List[Int63]): Int63 = {
    l match {
      case Nil() => { 0 }
      case Cons(hd, tl) => {
        
          if (
            tl == Nil()
          ) {
            hd 
          } else if (
            hd < max(tl)
          ) {
            max(tl) 
          } else {
            hd
          }
      }
    }
  }
   
}