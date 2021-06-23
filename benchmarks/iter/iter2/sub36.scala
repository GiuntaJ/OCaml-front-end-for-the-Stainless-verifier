import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub36 {
  /*problem 3*/
  def comp(((n, f)), x) = { if (n == 0) x else comp(n - 1, f, f(x)) } 
  
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case ((n, f), x) => { comp(n, f, x) }
  }
}