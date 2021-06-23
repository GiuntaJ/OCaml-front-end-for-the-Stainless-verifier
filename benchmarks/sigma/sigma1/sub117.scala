import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub117 {
  sealed case class NOMOVE(param0: String) extends Exception {}
  
  def sigma(((init, uppr, func))) = {
    
      if (
        init < uppr
      ) {
        func(init) + sigma(init + 1, uppr, func) 
      } else if (
        init eq uppr
      ) {
        func(init) 
      } else {
        0
      }
  }
    
}