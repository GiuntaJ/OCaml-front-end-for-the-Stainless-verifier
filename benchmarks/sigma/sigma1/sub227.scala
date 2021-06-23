import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub227 {
  /* 2006-11377 hw1-1 */
  
  def sigma(((start, finish, func))) = {
    if (start > finish) 0 else func(start) + sigma(start + 1, finish, func)
  }
}