import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub126 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case ((n, f), x) => { if (n == 1 || n == 0) f(x) else f(iter(n - 1, f, x)) }
  }
  
  /*
  iter(5,fun x -> 2+x) 0;;
  */
}