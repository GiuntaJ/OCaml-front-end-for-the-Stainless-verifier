import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub9 {
  /* problem 3*/
  
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case ((n, f), x) =>
      {
        val _2 = {
          def g(((n, f))) = { if (n eq 0) x else f(g(n - 1, f)) }
          g(n, f)
        }
    }
  } 
}