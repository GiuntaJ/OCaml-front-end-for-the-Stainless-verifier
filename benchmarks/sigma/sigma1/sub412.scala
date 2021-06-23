import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub412 {
  /*Computer Engineering 2015-12683 Kim Jaein Exercise 1-2*/
  /*let f n = n*n;;*/
  
  def sigma(((a: Int63, b: Int63, f: Int63 => Int63))) = {
    if (b < a) 0 else f(a) + sigma(a + 1, b, f)
  }
  /*
  let () = print_endline (string_of_int (sigma (1, 3, f)));;
  */
}