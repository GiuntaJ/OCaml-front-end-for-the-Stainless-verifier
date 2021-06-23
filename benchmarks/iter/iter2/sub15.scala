import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub15 {
  /* problem 3*/
  
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def impl(_n) = {
            if (_n == 0) ( (x) => { x } ) else ( (x) => { f(impl(_n - 1, x)) } )
          }
          impl(n)
        }
    }
  }
}