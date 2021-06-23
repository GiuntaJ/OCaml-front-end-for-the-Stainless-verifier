import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub148 {
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def recursive_function: (Int63, Int63) => Int63 = {
            case (p, cnt) =>
              {
                cnt match {
                  case 0 => { p }
                  case _ => { f(recursive_function(p, cnt - 1)) }
                }
            }
          }
          ( (x) => { recursive_function(x, n) } )
        }
    }
  }
}