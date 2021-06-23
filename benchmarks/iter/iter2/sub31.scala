import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub31 {
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) =>
      {
        val _2 = {
          def id_func(x) = { x }
          val _3 = {
            def compose(f, g, x) = { f(g(x)) }
            val _4 = {
              def iter_fun(n, f) = {
                if (n == 0) id_func else compose(f, iter_fun(n - 1, f))
              }
              
                if (
                  n == 0
                ) {
                  id_func 
                } else if (
                  n < 0
                ) {
                  assert(false, "Failure with n is negative value ") 
                } else {
                  iter_fun(n, f)
                }
            }
          }
        }
    }
  }
}