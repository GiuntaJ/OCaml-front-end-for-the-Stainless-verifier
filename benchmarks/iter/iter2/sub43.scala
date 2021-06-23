import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub43 {
  /* problem 3*/ val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def appl(n, k) = {
            
              if (
                n == 0
              ) {
                ( (x) => { k(x) } ) 
              } else {
                appl(n - 1, ( (x) => { f(k(x)) } ))
              }
          }
          if (n == 0) ( (x) => { x + 0 } ) else appl(n - 1, f)
        }
    }
  }
}