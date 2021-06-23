import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub107 {
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def func: (Int63 => Int63, Int63, Int63) => Int63 = {
            case (f, n) =>
              {
                
                  if (
                    n == 0
                  ) {
                    ( (x) => { x } ) 
                  } else {
                    ( (x) => { func(f, n - 1, f(x)) } )
                  }
            }
          }
          func(f, n)
        }
    }
  }
}