import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub1 {
  sealed case class Error(param0: String) extends Exception {}
  
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        assert(false, "Error with Garbage In ") 
      } else {
        b - a match {
          case 0 => { f(b) }
          case _ => { f(a) + sigma(a + 1, b, f) }
        }
      }
  }
}