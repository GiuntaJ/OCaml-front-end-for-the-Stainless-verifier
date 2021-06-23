import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub518 {
  
  /* Exercise 2*/
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f1) =>
      {
        
          if (
            a > b
          ) {
            0 
          } else if (
            a eq b
          ) {
            f1(b) 
          } else {
            f1(a) + sigma(a + 1, b, f1)
          }
    }
  } 
}