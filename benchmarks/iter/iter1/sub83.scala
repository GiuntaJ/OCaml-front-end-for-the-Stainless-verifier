import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub83 {
  def iter(((n, f))) = {
    if (n eq 0) ( (n) => { n } ) else ( (x) => { f(iter(n - 1, f, x)) } )
  }
  
}
