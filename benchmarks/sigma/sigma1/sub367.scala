import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub367 {
  
  /* SNU Programming Language Fall 2015
   * Homework 1 
   * Exercise 2: sigma
   * Written by Dongho Kang 
   */
  
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (a, b, f_n) =>
      {
        
          if (
            a eq b
          ) {
            f_n(a) 
          } else if (
            a > b
          ) {
            0 
          } else {
            f_n(a) + sigma(a + 1, b, f_n)
          }
    }
  }
}