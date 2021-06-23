import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub308 {
  /* 2010-11753 snucse Taekmin Kim */
  /* HW 1-2 */
  
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, fn) => { if (a > b) 0 else fn(a) + sigma(a + 1, b, fn) }
  }
  
  
  
  /* 
  let ta = 0
  let tb = 4
  let tfn : int -> int = fun x -> x
  
  let _ = print_endline(string_of_int(sigma(ta, tb, tfn)))
  
  let _ = print_endline(string_of_int(sigma (1, 10, fun x -> x)))
  
  let _ = 
    let print_bool x = 
      print_endline (string_of_bool x) in 
    print_bool (385 = sigma (1, 10, (fun x -> x * x))); 
    print_bool (0 = sigma (3, 1, fun x -> x * x)); 
    print_bool (27 = sigma(3, 3, fun x -> x * x * x)); 
    print_bool (385 = sigma(-10, -1, fun x -> x * x)) 
  */
}