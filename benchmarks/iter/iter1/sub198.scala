import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub198 {
  /* 2014-19180 You JooSeung Question 3*/
  
  def iter[A](((n: Int63, f: A => A)), x: A): A = {
    
      if (
        n == 0
      ) {
        x 
      } else if (
        n ne 1
      ) {
        f(iter(n - 1, f, x)) 
      } else {
        f(x)
      }
  }
}