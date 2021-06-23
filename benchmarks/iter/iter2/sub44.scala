import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub44 {
  /* problem 3*/
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        
          if (
            n == 0
          ) {
            ( (x) => { x } ) 
          } else {
            val _3 = {
              val main_func = f
              val _4 = {
                def help_iter(n, f) = {
                  
                    if (
                      n == 1
                    ) {
                      f 
                    } else {
                      help_iter(n - 1, ( (x) => { main_func(f(x)) } ))
                    }
                }
                help_iter(n, f)
              }
            }
          }
    }
  }
}