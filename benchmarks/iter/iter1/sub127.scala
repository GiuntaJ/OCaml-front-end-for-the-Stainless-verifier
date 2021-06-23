import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub127 {
  def iter(((n, f)), x) = { if (n <= 0) x else iter(n - 1, f, f(x)) }
  
  /*TESTCASE
  let twon n = iter(n, fun x -> 2+x) 0
  
  let _ = print_endline( string_of_int(twon 30))
  */
}