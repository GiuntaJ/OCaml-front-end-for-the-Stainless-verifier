import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub205 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
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
  )
   
}