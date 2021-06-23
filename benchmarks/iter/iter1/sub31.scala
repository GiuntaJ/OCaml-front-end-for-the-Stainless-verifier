import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub31 {
  /* 2008-11874 Lee, Sujee */
  /* EXERCISE 2 */
  
  def iter(((n, f)), x) = {
    val _2 = {
      def identity(x) = { x }
      
        if (
          n == 0
        ) {
          identity(x) 
        } else if (
          n == 1
        ) {
          f(x) 
        } else {
          iter(n - 1, f, f(x))
        }
    }
  }
  	
  	
  	/*
  let sum2 = iter(10, fun x -> 2+x) 0
  
  let _ = 
  	print_string "EXERCISE 2 : ";
  	print_int sum2;
  	print_newline()
  	*/
}