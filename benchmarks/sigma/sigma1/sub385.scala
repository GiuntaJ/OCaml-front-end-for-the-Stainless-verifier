import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub385 {
  def sigma: (Int63, Int63, (Int63 => Int63)) => Int63 = {
    case (i1, i2, fn) => { if (i1 > i2) 0 else fn(i1) + sigma(i1 + 1, i2, fn) }
  }
}