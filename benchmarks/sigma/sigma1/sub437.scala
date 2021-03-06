import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub437 {
  /*
   * 2017 - 09 - 11
   * PL Homework 1-2
   * Joonmo Yang
  */
  
  def sigma(((a, b, f))) = {
    
      if (
        a == b
      ) {
        f(a) 
      } else if (
        a > b
      ) {
        0 
      } else {
        f(a) + sigma(a + 1, b, f)
      }
  }
  
  /* tests
  let intdouble a =
    2*a
  
  let _ = print_int(sigma(1,10,intdouble))
  
  let _ = 
  let print_bool x = 
  print_endline (string_of_bool x) in 
  print_bool (385 = sigma (1, 10, (fun x -> x * x))); 
  print_bool (0 = sigma (3, 1, fun x -> x * x)); 
  print_bool (27 = sigma(3, 3, fun x -> x * x * x)); 
  print_bool (385 = sigma(-10, -1, fun x -> x * x)) 
  */
}