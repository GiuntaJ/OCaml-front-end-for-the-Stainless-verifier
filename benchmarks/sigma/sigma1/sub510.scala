import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub510 {
  def sigma(((a, b, f))) = { if (a > b) 0 else f(a) + sigma(a + 1, b, f) }
  
  /*
  let incr = fun x -> x+1
  let test1 = print_int(sigma(1, 9, incr)); print_newline();
  
  print_int(sigma (1, 10, (fun x -> x * x)));
  print_int (sigma (3, 1, fun x -> x * x));
  print_int (sigma(3, 3, fun x -> x * x * x));
  print_int (sigma(-10, -1, fun x -> x * x))
  */
}