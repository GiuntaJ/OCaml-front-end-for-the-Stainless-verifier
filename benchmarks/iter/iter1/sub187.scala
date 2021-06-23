import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub187 {
  def iter(((num: Int63, fnt: A => A))): A => A = {
    
      if (
        num eq 0
      ) {
        ( (x) => { x } ) 
      } else {
        val _3 = {
          def comp[A](f: A => A, g: A => A, x: A) = { f(g(x)) }
          comp(fnt, iter(num - 1, fnt))
        }
      }
  }
      
}