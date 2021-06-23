import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub10 {
  /* problem 3*/
  
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def idf(n) = { n }
          val _3 = {
            def help(n, f) = {
              
                if (
                  n eq 0
                ) {
                  idf 
                } else if (
                  n eq 1
                ) {
                  f 
                } else {
                  f
                }
            }
            help(n, f)
          }
        }
    }
  }
}