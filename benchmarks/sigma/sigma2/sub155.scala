import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub155 {
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) => { 0 }
  }
  /*| x :: 1 -> f x + sigmal a b*/
}