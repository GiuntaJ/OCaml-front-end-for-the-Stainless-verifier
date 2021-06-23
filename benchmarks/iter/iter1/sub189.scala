import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub189 {
  /*컴퓨터공학부 2014-16775 김민지
  programming language hw 1-3*/
  
  def iter(((n: Int63, f))) = {
    if (n <= 0) ( (y) => { y } ) else ( (y) => { iter(n - 1, f, f(y)) } )
  }
}