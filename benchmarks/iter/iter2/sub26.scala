import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub26 {
  /* problem 3*/
    val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def compose: ((Int63, (Int63 => Int63), (Int63 => Int63)), Int63) => Int63 = {
            case (n, f, g) =>
              {
                
                  if (
                    n == 0
                  ) {
                    ( (x) => { g(x) } ) 
                  } else {
                    compose(n - 1, f, ( (x) => { g(f(x)) } ))
                  }
            }
          }
          compose(n, f, ( (x) => { x } ))
        }
    }
  }
}