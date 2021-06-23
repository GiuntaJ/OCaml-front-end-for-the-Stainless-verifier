import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub405 {
  /* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 1-2 */
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) => { if (a > b) 0 else f(a) + sigma(a + 1, b, f) }
  }
  /* Test Code
  let x = 1
  let y = 10
  let f i = i * i
  let _ = print_endline (string_of_int (sigma (x,y,f)))
  */
}