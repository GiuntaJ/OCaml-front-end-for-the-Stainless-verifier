import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub125 {
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def iters: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
            case (a, fn) =>
              {
                a match {
                  case 0 => { ( (x) => { x } ) }
                  case 1 => { ( (x) => { fn(x) } ) }
                  case _ => { ( (x) => { iters(a - 1, fn, fn(x)) } ) }
                }
            }
          }
          iters(n, f)
        }
    }
  }
}