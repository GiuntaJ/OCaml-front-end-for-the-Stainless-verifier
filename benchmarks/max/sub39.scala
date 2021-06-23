import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub39 {
  /* Problem 3 */
  def length[A](l: List[A]): Int63 = {
    l match {
      case Nil() => { 0 }
      case Cons(hd, tl) => { 1 + length(tl) }
    }
  }
    
  def max: List[Int63] => Int63 = (
    (l) =>
      {
        l match {
          case Nil() => { assert(false, "Failure with error") }
          case Cons(hd, tl) => {
            
              if (
                length(l) == 1
              ) {
                hd 
              } else {
                val _3 = {
                  val a = max(tl)
                  if (hd > a) hd else a
                }
              }
          }
        }
    }
  )
    
}