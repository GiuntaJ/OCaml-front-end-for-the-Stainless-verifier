import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub458 {
  /*
    Department : Electrical Engineering
    Student-Id : 2008-11923
    Name : HyeonIL Choi (최현일)
    Date: 2017-9-7
    Homework-# : 1-2
    Excercise-Name : Sigma
  */
  
  def sigma(((a, b, f))) = {
    (a, b) match {
      case (a, b) => {
        
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
}
