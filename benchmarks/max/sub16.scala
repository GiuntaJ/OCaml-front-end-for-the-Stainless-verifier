import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub16 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { 0 }
          case Cons(hd, tl) => {
            
              if (
                tl == Nil()
              ) {
                hd 
              } else {
                val _3 = {
                  val big = max(tl)
                  if (hd > big) hd else big
                }
              }
          }
        }
    }
  )
   
}