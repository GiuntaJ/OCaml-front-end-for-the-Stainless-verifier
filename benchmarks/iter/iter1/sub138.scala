import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub138 {
  def iter: ((Int63, (A => A)), A) => A = {
    case (n, f) =>
      {
        val _2 = {
          def idf(i) = { i }
          
            if (
              n < 0
            ) {
              idf 
            } else if (
              n eq 0
            ) {
              idf 
            } else {
              ( (x) => { iter(n - 1, f, f(x)) } )
            }
        }
    }
  }
}