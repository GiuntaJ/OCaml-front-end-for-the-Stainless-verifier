import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub304 {
  def iter(((n: Int63, f))) = {
    if (n > 0) ( (x) => { f(iter(n - 1, f, x)) } ) else ( (x) => { x } )
  }
}