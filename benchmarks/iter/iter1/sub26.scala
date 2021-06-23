import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub26 {
  sealed case class OutofBound() extends Exception {}
  def iter(((n, f)), x) = {
    
      if (
        n > 1
      ) {
        iter(n - 1, f, f(x)) 
      } else if (
        n == 1
      ) {
        f(x) 
      } else if (
        n == 0
      ) {
        x 
      } else {
        assert(false, "OutofBound")
      }
  }
  
  /*let _= print_int(iter(10, fun x -> 2+x) 0)*/
}