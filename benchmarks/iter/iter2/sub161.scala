import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub161 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      { if (n > 0) iter(n - 1, ( (x) => { f(x) } )) else ( (x) => { x } )
    }
  }
      
  iter(3, ( (x) => { 2 + x } ), 0)
}