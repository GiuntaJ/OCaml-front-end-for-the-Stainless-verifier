import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub209 {
  def get_first[A, B, C](t: (A, B, C)): A = {
    t match {
      case (x, _, _) => { x }
    }
  }
  
  def get_second[A, B, C](t: (A, B, C)): B = {
    t match {
      case (_, x, _) => { x }
    }
  }
  
  def get_third[A, B, C](t: (A, B, C)): C = {
    t match {
      case (_, _, x) => { x }
    }
  }
  
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        0 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        sigma(a + 1, b, f) + f(a)
      }
  }
}