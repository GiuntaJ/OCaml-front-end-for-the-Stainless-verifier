import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub351 {
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) => { if (a > b) 0 else f(a) + sigma(a + 1, b, f) }
  }
  /*
  let add1 x = x+1
  let _ = print_endline (string_of_int (sigma (0, 3, add1)))
  */
}