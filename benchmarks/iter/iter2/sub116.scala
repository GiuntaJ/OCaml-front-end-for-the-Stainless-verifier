import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub116 {
  val idf: A => A = ( (x) => { x } )
  def compose(f, g, x) = { f(g(x)) }
  
  def iii(f, n) = { if (n == 0) idf else compose(f, iii(f, n - 1)) }
  
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) => { iii(f, n) }
  }
  
  
  
  iter(3, ( (x) => { 2 + x } ), 0)  
}
