import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub64 {
  /*problem 3*/
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def ite(n, f) = {
            if (n == 0) ( (x) => { x } ) else ( (x) => { ite(n - 1, f, f(x)) } )
          }
          ite(n, f)
        }
    }
  }
}