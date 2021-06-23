import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub252 {
  /*
   * 2017 - 09 - 11
   * PL Homework 1-3
   * Joonmo Yang
  */
  
  def iter(((n, f))) = {
    val _2 = {
      def iter_sub(((m, g, f_0))) = {
        
          if (
            m == 1
          ) {
            g 
          } else if (
            m < 1
          ) {
            g 
          } else {
            iter_sub(m - 1, ( (x) => { f_0(g(x)) } ), f_0)
          }
      }
      iter_sub(n, f, f)
    }
  }
  
  /* tests
  let fun2plus x = 2+x
  
  let _ = print_int (iter(9,fun2plus) 2)
  */
}