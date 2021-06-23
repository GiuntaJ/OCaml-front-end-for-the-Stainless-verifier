import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub305 {
  def compose(f, g, x) = { f(g(x)) }
  
  def iter(((n: Int63, f: A => A))): A => A = {
    if (n eq 0) ( (x) => { x } ) else compose(f, iter(n - 1, f))
  }
}
