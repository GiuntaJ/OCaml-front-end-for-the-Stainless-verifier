import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub239 {
  /* HW1-Exercise 3*/
  def iter(((n, f))) = {
    val _2 = {
      def iter2(n, f, res) = {
        if (n <= 0) res else iter2(n - 1, f, ( (x) => { f(res(x)) } ))
      }
      iter2(n, f, ( (x) => { x } ))
    }
  }
}