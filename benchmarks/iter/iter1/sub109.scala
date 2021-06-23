import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub109 {
  def iter(((n, func)), num) = {
    if (n > 0) func(iter(n - 1, func, num)) else num
  }
  	      
}