import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub121 {
  /* Department: Electrical and Computer Engineering */
  /* Student ID: 2010-11834 */
  /* Name: Kwonjoon Lee */
  /* Exercise #3 */
  def iter(((n, f))) = {
    if (n == 0) ( (x) => { x } ) else ( (x) => { f(iter(n - 1, f, x)) } )
  }
}