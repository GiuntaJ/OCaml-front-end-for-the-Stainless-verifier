import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub249 {
  def iter: ((Int63, (A => A)), A) => A = {
    case (n, f) =>
      {
        val _2 = {
          def iter2(n, f, r) = {
            
              if (
                n <= 0
              ) {
                r 
              } else {
                val _6 = {
                  def twice(x) = { f(f(x)) }
                  
                    if (
                      n % 2 eq 1
                    ) {
                      iter2(n / 2, twice, ( (x) => { f(r(x)) } )) 
                    } else {
                      iter2(n / 2, twice, r)
                    }
                }
              }
          }
          iter2(n, f, ( (x) => { x } ))
        }
    }
  }
}
