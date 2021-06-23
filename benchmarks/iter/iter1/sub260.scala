import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub260 {
  /*CSE/2011-11660/Kim Jiwoo/HW1-3*/
  def id[A](x: A): A = { x } /*identifier*/
  
  def compose(f, g, x) = { f(g(x)) } /*function composition*/
  
  def iter(((n: Int63, f))) = { if (n eq 0) id else compose(f, iter(n - 1, f)) }
}