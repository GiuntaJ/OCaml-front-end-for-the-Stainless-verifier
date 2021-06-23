import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub347 {
  val increase: Int63 => Int63 = ( (x) => { x + 1 } )
  def sigma(((x, y, f))) = { if (x > y) 0 else f(x) + sigma(x + 1, y, f) }
  
}
