import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_filter_sub87 {
  def filter(pred, lst, x, acc) = { if (pred(x)) x :: acc else acc }
}