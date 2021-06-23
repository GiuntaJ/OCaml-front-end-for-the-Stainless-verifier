import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub157 {
  /* Name : Jungwon Seo / Student ID : 2012210051 */ 
  
  sealed case class Problem() extends Exception {}
  
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { assert(false, "Problem") }
          case Cons(hd, Nil()) => { hd }
          case Cons(hd, tl) => { if (hd > max(tl)) hd else max(tl) }
        }
    }
  )
   
}