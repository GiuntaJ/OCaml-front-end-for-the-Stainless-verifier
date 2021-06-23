import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub137 {
  /* C:\Users\saigoy\Desktop\iter.ml */
  
  def iter: ((Int63, (A => A)), A) => A = {
    case ((n, f), init) => { if (n > 0) f(iter(n - 1, f, init)) else init }
  }
}
