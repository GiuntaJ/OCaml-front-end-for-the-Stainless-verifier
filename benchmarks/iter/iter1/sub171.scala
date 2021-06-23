import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub171 {
  def iter(((n, f))) = {
    val _2 = {
      def identity(x) = { x }
      n match {
        case 0 => { identity }
        case _ => { ( (x) => { iter(n - 1, f, f(x)) } ) }
      }
    }
  }
}