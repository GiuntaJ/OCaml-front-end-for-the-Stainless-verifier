import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub91 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def compose(f, g, x) = { f(g(x)) }
          n match {
            case 0 => { ( (x) => { x } ) }
            case 1 => { f }
            case _ => {
              val _5 = {
                val h = iter(n - 1, f)
                compose(f, h)
              }
            }
          }
        }
    }
  }
      
      
}