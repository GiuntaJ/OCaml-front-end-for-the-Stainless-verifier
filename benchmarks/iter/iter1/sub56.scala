import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub56 {
  /* hw 1_2. */
  def iter(((n, f)), a) = { if (n == 0) a else f(iter(n - 1, f, a)) }
  
  	/*
  let _ =
  	iter(10, function x -> 2 + x) 0
  	*/
}