import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub39 {
  /* problem 3*/
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n eq 0
          ) {
            val _3 = {
              def id(x) = { x }
              id
            } 
          } else {
            val _4 = {
              def compose(f, g, x) = { f(g(x)) }
              compose(f, iter(n - 1, f))
            }
          }
    }
  }
}