import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub34 {
  /* problem 3*/
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n < 0
          ) {
            assert(false, "Failure with n has to be positive ") 
          } else {
            n match {
              case 0 => { ( (x) => { x } ) }
              case _ => {
                val _2 = {
                  def compose_func(f, g, x) = { f(g(x)) }
                  compose_func(f, iter(n - 1, f))
                }
              }
            }
          }
    }
  }
  	  /*The function "compose_func" is mapped because the fucntion "iter" is a function that composes itself. And we need to call the function "iter" recursively til n becomes 0*/
}