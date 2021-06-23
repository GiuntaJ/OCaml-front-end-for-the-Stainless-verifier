import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub233 {
  
  def iter(((n: Int63, f: Int63 => Int63)), x: Int63): Int63 = {
    if (n <= 0) x else f(iter(n - 1, f, x))
  }
  
  /*
    let a31 = iter (3, function x -> 2+x) 0
    let a32 = iter (0, function x -> 2*x) 4
    let a33 = iter (11, function x -> 2*x+1) 7 */
}