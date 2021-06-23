import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub509 {
  /*자유전공학부 2013-13444 박하영*/
  
  def sigma(((a: Int63, b: Int63, f: Int63 => Int63))): Int63 = {
    if (b < a) 0 else f(a) + sigma(a + 1, b, f)
  }
}