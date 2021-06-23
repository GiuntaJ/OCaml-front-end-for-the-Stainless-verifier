import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub2 {
  
  /* problem 3*/
  
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case ((n, f), k) =>
      {
        val _2 = {
          def loop(x) = { if (x == 0) k else f(loop(x - 1)) }
          loop(n)
        }
    }
  }
}