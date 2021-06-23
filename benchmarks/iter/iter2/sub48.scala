import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub48 {
  /* problem 3*/
  
  def id[A](x: A): A = { x }
  def compose(f, g, x) = { f(g(x)) }
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n == 0
          ) {
            id 
          } else if (
            n == 1
          ) {
            f 
          } else {
            compose(f, iter(n - 1, f))
          }
    }
  }
}