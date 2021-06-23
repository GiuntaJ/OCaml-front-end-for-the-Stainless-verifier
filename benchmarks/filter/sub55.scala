import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_filter_sub55 {
  /********************/
  /* Problem 1: filter */
  /********************/
  def filter: (A => Boolean, List[A]) => List[A] = {
    case (pred, lst) =>
      {
        
          if (
            lst.length > 0
          ) {
            
              if (
                pred(lst.head)
              ) {
                lst.head :: filter(pred, lst.tail) 
              } else {
                filter(pred, lst.tail)
              } 
          } else {
            Nil()
          }
    }
  } 
}