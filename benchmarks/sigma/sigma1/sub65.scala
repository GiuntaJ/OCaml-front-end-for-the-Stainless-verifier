import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub65 {
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) =>
      {
        
          if (
            a < b
          ) {
            sigma(a, b - 1, f) + f(b) 
          } else if (
            a == b
          ) {
            f(b) 
          } else {
            assert(false, "Invalid_argument with sigma")
          }
    }
  }
  /*
  let _ = let result = sigma(1,5,fun x -> x + 2) in print_int result;
  	print_newline();
  */
}