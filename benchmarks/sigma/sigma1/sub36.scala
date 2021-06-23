import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub36 {
  /* Exercise 1 */
  sealed case class Error(param0: String) extends Exception {}
  
  def sigma(((a, b, f))) = {
    a - b match {
      case 0 => { f(a) }
      case _ => {
        
          if (
            a > b
          ) {
            assert(false, "Error with Invalid arguments : a is larger than b ") 
          } else {
            f(a) + sigma(a + 1, b, f)
          }
      }
    }
  }
}