import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub12 {
  /* problem 3*/
  def fst(((x, _))) = { x }
  
  def snd(((_, x))) = { x }
  
  def compose(f, g, x) = { f(g(x)) }
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      { if (n == 0) ( (x) => { 1 * x } ) else compose(f, iter(n - 1, f))
    }
  }
}