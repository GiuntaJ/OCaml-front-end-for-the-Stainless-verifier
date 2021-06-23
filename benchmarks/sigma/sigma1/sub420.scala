import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub420 {
  
  def sigma(((a: Int63, b: Int63, f: Int63 => Int63))): Int63 = {
    if (a > b) 0 else f(a) + sigma(a + 1, b, f)
  }
  
  /* let test = sigma (5,4,(fun x -> x*x))
  
  let _ = print_endline (string_of_int test) */
  /*
  let _ =
  let print_bool x =
  print_endline (string_of_bool x) in
  print_bool (385 = sigma (1, 10, (fun x -> x * x)));
  print_bool (0 = sigma (3, 1, fun x -> x * x));
  print_bool (27 = sigma(3, 3, fun x -> x * x * x));
  print_bool (385 = sigma(-10, -1, fun x -> x * x))  */
}