import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub180 {
  def iter_0(((n, f))) = {
    if (n == 0) ( (x) => { x } ) else ( (x) => { iter_0(n - 1, f, f(x)) } )
  }
       
  def iter(((n, f))) = { iter_0(n, f) } 
}