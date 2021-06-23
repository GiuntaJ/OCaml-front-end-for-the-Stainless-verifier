import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub99 {
  /*Problem 1*/
  val max_f: (Int63, Int63) => Int63 = {
    case (a, b) => { if (a > b) a else b }
  }
  
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { -(99999) }
          case Cons(hd, tl) => { max_f(hd, max(tl)) }
        }
    }
  ) 
   
}