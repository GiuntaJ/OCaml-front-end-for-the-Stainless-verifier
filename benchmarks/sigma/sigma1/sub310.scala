import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub310 {
  /* Department: Electrical and Computer Engineering */
  /* Student ID: 2010-11834 */
  /* Name: Kwonjoon Lee */
  /* Exercise #2 */
  def sigma(t: (Int63, Int63, (Int63 => Int63))): Int63 = {
    t match {
      case (a, b, f) => { if (a > b) 0 else f(b) + sigma(a, b - 1, f) }
    }
  }
}