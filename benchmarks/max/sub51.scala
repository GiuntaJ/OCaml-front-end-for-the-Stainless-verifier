import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub51 {
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { assert(false, "Failure with list is too short") }
          case Cons(h, t) => {
            
              if (
                t == Nil()
              ) {
                h 
              } else if (
                h >= max(t)
              ) {
                h 
              } else {
                max(t)
              }
          }
        }
    }
  )
   
}