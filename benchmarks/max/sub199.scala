import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub199 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = ( (lst) => { 0 } ) 
  
  
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        val _4 = {
          def rmax: (Int63, List[Int63]) => Int63 = {
            case (m, l) =>
              {
                l match {
                  case Nil() => { m }
                  case Cons(x, xs) => { if (m > x) rmax(m, xs) else rmax(x, xs)
                  }
                }
            }
          }
          lst match {
            case Cons(x, xs) => { rmax(x, xs) }
          }
        }
    }
  )
  
   
}