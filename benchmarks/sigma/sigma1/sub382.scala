import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub382 {
  def sigma(((a, b, f))) = {
    val _2 = {
      def sigma_rec(((n, ret))) = {
        if (n <= b) sigma_rec(n + 1, ret + f(n)) else ret
      }
      sigma_rec(a, 0)
    }
  }
  
  /*
  let raw x = x
  
  let _ = print_int (sigma (1,10,raw))
  */
}