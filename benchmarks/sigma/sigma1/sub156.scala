import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub156 {
  
  def sigma(((st, ed, f))) = {
    
      if (
        st > ed
      ) {
        0 
      } else if (
        st == ed
      ) {
        f(st) 
      } else {
        f(st) + sigma(st + 1, ed, f)
      }
  }
  
  
  /*	
  let _ = print_int (sigma(1,5, fun x -> x+2));
  	print_newline()
  
  
  let _ = print_int (sigma(5,5, fun x -> x+2));
  	print_newline()
  let t1 = (sigma(5,4, fun x -> x+2))
  let _ = print_int(t1);
  	print_newline()
  */	
}