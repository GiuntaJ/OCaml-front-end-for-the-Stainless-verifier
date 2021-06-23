import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub189 {
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = (
    (x) =>
      {
        x match {
          case (a, b, f) => {
            
              if (
                a > b
              ) {
                0 
              } else if (
                a == b
              ) {
                f(b) 
              } else {
                f(a) + sigma(a + 1, b, f)
              }
          }
        }
    }
  )
  
}
