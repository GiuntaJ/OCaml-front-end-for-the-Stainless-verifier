import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub164 {
  def iter(((n, f))) = {
    n match {
      case 0 => { ( (k) => { k } ) }
      case 1 => { f }
      case x => { ( (k) => { f(iter(x - 1, f, k)) } ) }
    }
  }
  /*
  let du = iter (10, fun x -> 2+x)
  let _ = print_endline (string_of_int (du 2) )
  */
}