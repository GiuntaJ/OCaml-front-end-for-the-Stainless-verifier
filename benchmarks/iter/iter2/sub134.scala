import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub134 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def compose(g, f, x) = { g(f(x)) }
          if (n == 0 || n == 1) f else compose(f, iter(n - 1, f))
        }
    }
  }
  
  
  iter(3, ( (x) => { 2 + x } ), 0)
}