import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub177 {
  /* SNU Programming Language Fall 2015
   * Homework 1 
   * Exercise 3: iter
   * Written by Dongho Kang 
   */
  def iter: ((Int63, (A => A)), A) => A = {
    case (n, f) =>
      {
        val _2 = {
          def f_i(x) = { x }
          
            if (
              n eq 0
            ) {
              f_i 
            } else if (
              n < 0
            ) {
              f_i 
            } else {
              val _3 = {
                def f_n(x) = { f(iter(n - 1, f, x)) }
                f_n
              }
            }
        }
    }
  }
}