import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub142 {
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n == 0
          ) {
            ( (a) => { a } ) 
          } else {
            val _3 = {
              def foo(n, f) = {
                
                  if (
                    n == 1
                  ) {
                    ( (a) => { f(a) } ) 
                  } else {
                    ( (a) => { foo(n - 1, f, f(a)) } )
                  }
              }
              foo(n, f)
            }
          }
    }
  }
}