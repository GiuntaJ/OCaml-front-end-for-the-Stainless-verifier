import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub105 {
  def iter(((n, f))) = {
    if (n > 0) ( (x) => { iter(n - 1, f, f(x)) } ) else ( (x) => { x } )
  }
}