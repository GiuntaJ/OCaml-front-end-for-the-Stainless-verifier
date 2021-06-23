import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub315 {
  def sigma(((a, b, f))) = { if (a > b) 0 else f(a) + sigma(a + 1, b, f) }
  
  /*TESTCASE
   let _ = print_endline (string_of_int (sigma(4, 4, fun x -> 2*x))) */
}