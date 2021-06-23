import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub227 {
  /*
      Homework 1, Exercise 3
      2015-15894 Jonghoon Won
      Sep 14, 2017
  */
  
  def iter: ((Int63, (A => A)), A) => A = {
    case (n, f) =>
      { if (n <= 0) ( (x) => { x } ) else ( (x) => { iter(n - 1, f, f(x)) } )
    }
  }
}