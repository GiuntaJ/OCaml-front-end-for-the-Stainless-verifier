import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub214 {
  /* 2015-11380 박찬양 HW1-3 */
  
  def iter: ((Int63, (A => A)), A) => A = {
    case ((n, func), x) =>
      {
        val _2 = {
          val nx = func(x)
          n match {
            case 0 => { x }
            case _ => { iter(n - 1, func, nx) }
          }
        }
    }
  }
}