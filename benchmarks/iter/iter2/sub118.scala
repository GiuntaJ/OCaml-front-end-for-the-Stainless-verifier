import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub118 {
  val identity: A => A = ( (x) => { x } )
  
  val compose: (A => B, C => A, C) => B = {
    case (f, g, x) => { f(g(x)) }
  }
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) => { if (n == 0) identity else compose(f, iter(n - 1, f)) }
  }
    
    
  iter(5, ( (x) => { 2 + x } ), 0)
  iter(10, ( (x) => { x * 2 } ), 1)
}