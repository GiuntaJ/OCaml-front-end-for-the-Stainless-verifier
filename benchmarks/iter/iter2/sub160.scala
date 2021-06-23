import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub160 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        n match {
          case 1 => { f }
          case _ => {
            val _2 = {
              val g = iter(n - 1, f)
              ( (x) => { f(g(x)) } )
            }
          }
        }
    }
  }
}