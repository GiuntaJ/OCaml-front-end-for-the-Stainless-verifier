import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub273 {
  /* 4190,310 Programming Language (Fall 2014)
   * Homework 1 - Exercise 1
   * CSE / 2012-13456 / Gao, Chengbin */
  
  def sigma(((a, b, f))) = { if (a >= b) f(a) else sigma(a + 1, b, f) + f(a) }
}