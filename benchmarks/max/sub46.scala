import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub46 {
  
  /* Problem 3 */
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Cons(hd, tl) => {
            
              if (
                tl == Nil()
              ) {
                hd 
              } else {
                val _3 = {
                  val mx = max(tl)
                  if (hd > mx) hd else mx
                }
              }
          }
        }
    }
  )
  			  
    
}