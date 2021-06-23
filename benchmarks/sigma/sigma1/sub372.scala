import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub372 {
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f) =>
      {
        b match {
          case 0 => { 0 }
          case _ => {
            
              if (
                b < a
              ) {
                0 
              } else if (
                b == a
              ) {
                f(a) 
              } else {
                f(a) + sigma(a + 1, b, f)
              }
          }
        }
    }
  }
  
}
