import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_max_sub112 {
  /********************/
  /*     Problem 1     */
  /********************/
  def max: List[Int63] => Int63 = (
    (lst) =>
      {
        lst match {
          case Nil() => { failwith("Empty list") }
          case Cons(head, tail) => {
            
              if (
                lst.length == 1
              ) {
                head 
              } else if (
                head >= tail.apply(0)
              ) {
                max(head :: tail.tail) 
              } else {
                max(tail)
              }
          }
        }
    }
  )
  
   
}