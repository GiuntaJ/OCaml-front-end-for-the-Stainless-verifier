import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub60 {
  /* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#1-1 */
  
  def sigma(((a, b, f))) = { if (a > b) 0 else sigma(a + 1, b, f) + f(a) }
  
  /*
  /* test code */
  let square a = a * a
  
  let _ = print_endline (string_of_int (sigma (1, 5, square)))
  */
}