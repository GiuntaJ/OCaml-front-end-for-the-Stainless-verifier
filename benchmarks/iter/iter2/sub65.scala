import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub65 {
  /* problem 3*/
  
  val compose: (Int63 => Int63, Int63 => Int63, Int63) => Int63 = {
    case (f, g, n) => { f(g(n)) }
  }
  
  def iter_helper: (Int63, Int63 => Int63, Int63 => Int63, Int63) => Int63 = {
    case (n, prev, f) =>
      { if (n == 0) prev else iter_helper(n - 1, compose(prev, f), f)
    }
  }
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n == 0
          ) {
            ( (x) => { x } ) 
          } else if (
            n < 0
          ) {
            assert(
              false,
              "Failure with             ValueError: n must be a non-negative integer. ") 
          } else {
            iter_helper(n - 1, f, f)
          }
    }
  }
}