import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub210 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Cons(hd, Nil()) => { hd }
          case Cons(hd, Cons(hd_0, tl)) => {
            if (hd > hd_0) max(hd :: tl) else max(hd_0 :: tl)
          }
        }
    }
  )
  
   
}