import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub137 {
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case ((n, f), m) =>
      {
        val _2 = {
          def loop(n, f) = { if (n < 2) f(m) else f(m) + loop(n - 1, f) }
          loop(n, f)
        }
    }
  }
  
  iter(12, ( (x) => { 2 + x } ), 2)
}