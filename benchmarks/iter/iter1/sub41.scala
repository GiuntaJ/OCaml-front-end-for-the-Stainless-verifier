import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub41 {
  def iter(((n, f)), k) = {
    
      if (
        n < 0
      ) {
        assert(false, "Invalid_argument with error ") 
      } else if (
        n == 0
      ) {
        ( (x) => { x } )(k) 
      } else {
        f(iter(n - 1, f, k))
      }
  }
  
  /*
  let double s = s ^ s;;
  
  print_string( iter(3, double) "a" );;
  print_newline();;
  */
}