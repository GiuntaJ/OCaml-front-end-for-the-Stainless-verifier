import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub95 {
  /* hw 1_1. */
  def sigma(((a, b, f))) = { if (a == b) f(a) else f(a) + sigma(a + 1, b, f) }
  
  	/*
  let _ =
  	sigma(1, 10, (function x -> 2 * x))
  	*/
}