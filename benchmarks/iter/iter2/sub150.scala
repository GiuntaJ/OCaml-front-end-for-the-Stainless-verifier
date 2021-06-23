import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub150 {
  def compose(f, g, x) = { f(g(x)) }
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        n match {
          case 0 => { f }
          case 1 => { f }
          case _ => { compose(f, iter(n - 1, f)) }
        }
    }
  }
  
  /* Test Cases
  iter(0, fun x -> 2+x) 0;;
  iter(1, fun x -> 2+x) 0;;
  iter(2, fun x -> 2+x) 0;;
  iter(3, fun x -> 2+x) 0;;
  */
}