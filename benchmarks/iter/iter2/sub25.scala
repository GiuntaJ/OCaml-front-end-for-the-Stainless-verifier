import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub25 {
  /*problem 3. */
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def iter_f(f, g, x) = { f(g(x)) }
          if (n == 0) ( (x) => { x } ) else iter_f(f, iter(n - 1, f))
        }
    }
  } 
}