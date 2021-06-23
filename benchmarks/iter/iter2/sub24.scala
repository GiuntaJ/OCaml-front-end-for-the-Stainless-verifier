import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub24 {
  /*problem 3*/ def mul(f, n, a) = { if (n == 0) ( (x) => { x } )(a) else mul(f, n - 1, f(a))
  }
    val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) => { mul(f, n) }
  }
}