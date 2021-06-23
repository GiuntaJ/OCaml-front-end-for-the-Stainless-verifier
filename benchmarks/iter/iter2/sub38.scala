import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub38 {
  /* problem 3 */ 
  def compose(f: Int63 => Int63, g: Int63 => Int63, x: Int63) = { f(g(x)) }
  def all_compose(n, f: Int63 => Int63) = {
    if (n == 1) f else compose(f, all_compose(n - 1, f))
  }
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        n match {
          case 0 => { ( (x) => { x } ) }
          case _ => { all_compose(n, f) }
        }
    }
  }
}