import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub33 {
  /* 2004-11951 Noh, Soon Hyun */
  
  def iter(((n, f)), x) = { if (n <= 0) x else iter(n - 1, f, f(x)) }
  
  /* Test Code ::
  let square n = n*n
  let _ = print_int (iter(0, function x -> 2+x) (-3)); print_char '
  '
  let _ = print_int (iter(7, function x -> 2+x) 0); print_char '
  '
  let _ = print_int (iter(3, square) 2); print_char '
  '
  let _ = print_int (iter(10, function x -> 2*x) 1); print_char '
  '
  */
}