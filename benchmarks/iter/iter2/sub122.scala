import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub122 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      { if (n == 0) ( (k) => { k } ) else ( (k) => { iter(n - 1, f, f(k)) } )
    }
  }
  
  iter(7, ( (x) => { 2 + x } ), 0)
  iter(7, ( (x) => { 3 + x } ), 0)
}