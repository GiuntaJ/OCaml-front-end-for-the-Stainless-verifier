import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub144 {
  def compose(f, g, x) = { f(g(x)) }
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        n match {
          case 0 => { ( (x) => { x } ) }
          case _ => { compose(f, iter(n - 1, f)) }
        }
    }
  }
}