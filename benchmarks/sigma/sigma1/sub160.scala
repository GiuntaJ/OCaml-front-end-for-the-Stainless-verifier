import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub160 {
  def sigma(((a, b, f))) = { if (a > b) 0 else f(a) + sigma(a + 1, b, f) }
  
  /* base case 1 */
  val t1: Boolean = 10 eq sigma(10, 10, ( (x) => { x } ))
  val t2: Boolean = sigma(11, 10, ( (x) => { x } )) eq 0
  val t3: Boolean = sigma(1, 10, ( (x) => { x } )) eq 55
  val t4: Boolean = sigma(1, 10, ( (x) => { if (x % 2 == 0) 1 else 0 } )) eq 5
  val t5: Boolean = sigma(1, 10, ( (x) => { x * x } )) eq 385
}
