import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub27 {
  sealed case class Error_of_string() extends Exception {}
  
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        assert(false, "Error_of_string") 
      } else if (
        a == b
      ) {
        f(b) 
      } else {
        f(a) + sigma(a + 1, b, f)
      }
  }
  
  /*
  let f = function a -> a*2;;
  let g a = a*3;;
  let (h : int -> int) = function a -> a*4;;
  */
}