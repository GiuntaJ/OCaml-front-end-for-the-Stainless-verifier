import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub63 {
  /* problem 3*/
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case ((n, f), x) =>
      {
        n match {
          case 0 => { x }
          case _ => { f(iter(n - 1, f, x)) }
        }
    }
  } 
}