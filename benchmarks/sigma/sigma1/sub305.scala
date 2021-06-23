import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub305 {
  /* Ex 2. Sigma */
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        0 
      } else if (
        a eq b
      ) {
        f(a) 
      } else {
        f(a) + sigma(a + 1, b, f)
      }
  }
  /*
  let _ =
  	let msg = string_of_int (sigma (1, 3, (fun x -> 2 * x))) in
  	print_endline msg
  
  let _ =
  	let msg = string_of_int (sigma (1, 3, (fun x -> x * x))) in
  	print_endline msg
  */
}