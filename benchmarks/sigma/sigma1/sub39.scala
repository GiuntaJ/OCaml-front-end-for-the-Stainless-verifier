import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub39 {
  sealed case class Error(param0: String) extends Exception {}
  
  def sigma(((a, b, f))) = {
    
      if (
        a < b
      ) {
        f(a) + sigma(a + 1, b, f) 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        assert(false, "Error with index error: a shouldn't be larger than b")
      }
  } 
  
  def square(a: Int63): Int63 = { a * a }
  def identity[A](a: A): A = { a }
}