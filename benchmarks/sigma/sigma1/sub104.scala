import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub104 {
  /* 2008-11874 EXERCISE 2 */
  
  def sigma(((a, b, f))) = {
    
      if (
        b > a
      ) {
        f(b) + sigma(a, b - 1, f) 
      } else if (
        b == a
      ) {
        f(b) 
      } else {
        0
      }
  }
  	
  	
  	
  /*let sum = sigma(1,5,fun n -> n+1)
  
  let _ = 
  	print_string "EXERCISE 1 : ";
  	print_int sum;
  	print_newline()
  	*/
}