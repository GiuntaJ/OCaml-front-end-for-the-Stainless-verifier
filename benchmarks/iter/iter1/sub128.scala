import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub128 {
  /*2007-10575 조용훤*/
  def iter(((n, f)), r) = {
    val _2 = {
      val a = fst(n, f)
      val _3 = {
        val b = snd(n, f)
        
          if (
            a > 1
          ) {
            b(r) + iter(n - 1, f, r) 
          } else if (
            a == 1
          ) {
            b(r) 
          } else {
            r
          }
      }
    }
  }
}