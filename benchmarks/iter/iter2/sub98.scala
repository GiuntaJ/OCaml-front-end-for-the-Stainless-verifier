import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub98 {
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def temp_f(cnt, f, a) = {
            if (cnt <= 0) a else f(temp_f(cnt - 1, f, a))
          }
          temp_f(n, f)
        }
    }
  }
  
  iter(11, ( (x) => { x + 2 } ), 0)
  iter(3, ( (x) => { x * x } ), 2)
}