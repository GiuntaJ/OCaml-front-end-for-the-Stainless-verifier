import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub72 {
  /*problem 3*/
    def compose(f: Int63 => Int63, g: Int63 => Int63, x: Int63): Int63 = { f(g(x))
  }
    def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      { if (n == 0) ( (x) => { x } ) else compose(f, iter(n - 1, f))
    }
  }
}