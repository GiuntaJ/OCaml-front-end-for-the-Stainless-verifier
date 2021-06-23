import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub19 {
  /* problem 3*/
  
  def compose(f, g, x) = { f(g(x)) }
  
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        n match {
          case 1 => { f }
          case _ => {
            val _2 = {
              val asdf = iter(n - 1, f)
              compose(f, asdf)
            }
          }
        }
    }
  }
}