import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub101 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      { if (n eq 0) ( (x) => { x } ) else ( (x) => { f(iter(n - 1, f, x)) } )
    }
  }
        
  iter(8, ( (x) => { 2 + x } ), 0)
}