import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub225 {
  /* file name : ex1.ml */
  /* author : Jisoon Park (jspark@ropas.snu.ac.kr) */
  /* date : 2013-09-13 */
  /* Exercise 1 */
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) =>
      {
        
          if (
            a > b
          ) {
            0 
          } else if (
            a == b
          ) {
            f(a) 
          } else {
            f(a) + sigma(a + 1, b, f)
          }
    }
  }
}