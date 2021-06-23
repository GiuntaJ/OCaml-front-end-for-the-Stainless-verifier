import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub88 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      { if (n == 0) ( (n) => { n } ) else ( (x) => { f(iter(n - 1, f, x)) } )
    }
  }
  iter(0, ( (x) => { 2 + x } ), 0)
  iter(20, ( (x) => { 2 + x } ), 0)
  iter(123, ( (x) => { 2 + x } ), 0)
}