import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub183 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { -(2000000000) }
          case Cons(hd, tl) => { if (hd > max(tl)) hd else max(tl) }
        }
    }
  ) 
   
}