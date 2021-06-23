import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub98 {
  sealed case class Negative_int() extends Exception {}
  
  def iter(((n: Int63, f: A => A))) = {
    
      if (
        n < 0
      ) {
        assert(false, "Negative_int") 
      } else if (
        n == 0
      ) {
        ( (x) => { x } ) 
      } else {
        ( (x) => { f(iter(n - 1, f, x)) } )
      }
  }
}