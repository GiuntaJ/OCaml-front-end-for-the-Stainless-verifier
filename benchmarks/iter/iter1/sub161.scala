import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub161 {
  /*컴공 2014-10618 이세영 1-3*/
  def iter(((n, f))) = {
    val _2 = {
      def com(((nn, ff)), y) = { if (nn > 0) ff(com(nn - 1, ff, y)) else y }
      ( (x) => { com(n, f, x) } )
    }
  }
}