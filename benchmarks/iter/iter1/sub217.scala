import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub217 {
  /* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 1-3 */
  def iter(((n, f)), x) = {
    n match {
      case 0 => { ( (y) => { y } )(x) }
      case _ => { f(iter(n - 1, f, x)) }
    }
  }
  /* Test Code
  let n = 7
  let _ = print_endline (string_of_int(iter(n, function x -> 2+x) 0))
  let _ = print_endline (string_of_int(iter(4, function x -> 3+x) 1))
  let _ = print_endline (string_of_int(iter(4, function x -> 3 + 2*x) 1))
  let _ = print_endline (string_of_int(iter(n, function x -> 2*x) 1))
  let _ = print_endline (string_of_int(iter(0, function x -> 3+x) 5))
  let _ = print_endline (iter(n, function x -> x ^ "*") "#")
  let _ = print_endline (iter(n, function x -> "*" ^ x) "#")
  */
}