import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub141 {
  def sigma(((lower, upper, func))) = {
    
      if (
        lower <= upper
      ) {
        
          if (
            lower == upper
          ) {
            func(lower) 
          } else {
            func(lower) + sigma(lower + 1, upper, func)
          } 
      } else {
        0
      }
  }
  	
  /* TEST SET */
  /*
  let _ =
  	print_string "sigma Test Set
  ";
  	print_int (sigma (1, 1, (function x -> x)));
  	print_char '
  ';
  	print_int (sigma (1, 10, (function x -> x)));
  	print_char '
  ';
  	print_int (sigma (-50, 50, (function x -> x * x)));
  	print_char '
  ';
  	print_int (sigma (2, 10, (function x -> 10)));
  	print_string "
  
  "
  */
}