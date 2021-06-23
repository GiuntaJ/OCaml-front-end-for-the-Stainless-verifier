import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub378 {
  /* ex 2 */
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) => { if (a > b) 0 else f(a) + sigma(a + 1, b, f) }
  }
  
  /* ex 2 test */
  /*
  let square x = x * x
  let added = sigma (1, 5, square)
  let _ = print_endline(string_of_int added)
  */
}