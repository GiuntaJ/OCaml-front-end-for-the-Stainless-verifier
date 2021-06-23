import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub366 {
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) => { if (a > b) 0 else f(b) + sigma(a, b - 1, f) }
  }
  /*
  let f : int -> int = fun x -> x+3
  let a : int = sigma(0,8,f)
  let _ = print_endline(string_of_int a)
  
  
  let _ = 
  let print_bool x = 
  print_endline (string_of_bool x) in 
  print_bool (385 = sigma (1, 10, (fun x -> x * x))); 
  print_bool (0 = sigma (3, 1, fun x -> x * x)); 
  print_bool (27 = sigma(3, 3, fun x -> x * x * x)); 
  print_bool (385 = sigma(-10, -1, fun x -> x * x)) 
  */
}