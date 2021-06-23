import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub230 {
  val sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (fromIndex, toIndex, f) =>
      {
        val _2 = {
          def sigma_aux: (Int63, Int63) => Int63 = {
            case (currentIndex, result) =>
              {
                
                  if (
                    currentIndex > toIndex
                  ) {
                    result 
                  } else {
                    sigma_aux(currentIndex + 1, result + f(currentIndex))
                  }
            }
          }
          sigma_aux(fromIndex, 0)
        }
    }
  }
}