import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub275 {
  def iter(((n: Int63, func: A => A))): A => A = {
    if (n eq 0) ( (x) => { x } ) else ( (x) => { iter(n - 1, func, func(x)) } )
  }
}