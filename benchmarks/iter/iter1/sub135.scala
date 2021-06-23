import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub135 {
  def comp(f, g, x) = { f(g(x)) }
  
  def iter(((n, f))) = {
    if (n == 0) ( (x) => { x } ) else comp(f, iter(n - 1, f))
  }
}