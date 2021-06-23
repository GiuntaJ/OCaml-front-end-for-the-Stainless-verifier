import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub96 {
  /*snucse, 200611810, exercise01*/
  
  sealed case class Error(param0: Int63,  param1: Int63) extends Exception {}
  
  def sigma(((a, b, f))) = {
    
      if (
        a == b
      ) {
        f(a) 
      } else if (
        a < b
      ) {
        f(a) + sigma(a + 1, b, f) 
      } else {
        assert(false, "Error with a, b")
      }
  }
  
  /*
  let f x = x+1
  let re = sigma (1,2,f)
  let _ =
  print_int re
  */
}
