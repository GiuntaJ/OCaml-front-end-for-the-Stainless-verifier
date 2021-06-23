import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub306 {
  def iter[A](x: (Int63, (A => A)), arg: A): A = {
    val _2 = {
      val ((n, f)) = x
      
        if (
          n <= 0
        ) {
          arg 
        } else if (
          n eq 1
        ) {
          f(arg) 
        } else {
          iter(n - 1, f, f(arg))
        }
    }
  }
}