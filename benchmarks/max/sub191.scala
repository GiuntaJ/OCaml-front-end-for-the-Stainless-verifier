import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub191 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { assert(false, "Failure with The List is Empty") }
          case Cons(hd, tl) => {
            
              if (
                tl == Nil()
              ) {
                hd 
              } else {
                val _3 = {
                  def maxcom(x, y) = { if (x > y) x else y }
                  maxcom(hd, max(tl))
                }
              }
          }
        }
    }
  )
   
}