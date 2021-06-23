import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub73 {
  /* let c=1
     let d=10
     let incr = fun x -> x+1 */
  
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        0 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        f(a) + sigma(a + 1, b, f)
      }
  }
  
  /* let test = sigma(c,d, incr)
  
  let _ =
  print_int test;
  print_newline() */
}