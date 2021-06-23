import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub141 {
  val iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          val f_original = f
          val _3 = {
            def iter2(((n, f))) = {
              
                if (
                  n == 0
                ) {
                  ( (x) => { x } ) 
                } else if (
                  n == 1
                ) {
                  f 
                } else {
                  iter2(n - 1, ( (x) => { f_original(f(x)) } ))
                }
            }
            iter2(n, f)
          }
        }
    }
  }
    
  iter(10, ( (x) => { x * 2 } ), 5)
}