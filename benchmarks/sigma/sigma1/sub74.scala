import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub74 {
  /* 2004-11951 Noh, Soon Hyun */
  
  /* Didn't handle exception case according to a TA Yoon's comment on Web Board */
  /* I'll be used to it next time T^T */
  def sigma(((a, b, f))) = { if (a == b) f(a) else f(a) + sigma(a + 1, b, f) }
  
  /* Test Code ::
  let original n = n
  let square n = n * n
  let quad n = n * n * n
  let _ = print_int (sigma (1, 1, original)); print_char '
  '
  let _ = print_int (sigma (1, 5, square)); print_char '
  '
  let _ = print_int (sigma (-51, 51, quad)); print_char '
  '
  */
}