import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub459 {
  def sigma(((beg: Int63, fin: Int63, f: Int63 => Int63))): Int63 = {
    if (beg > fin) 0 else f(beg) + sigma(beg + 1, fin, f)
  }
}