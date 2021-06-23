import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub290 {
  
  /*
      컴퓨터 공학부 2012-11270 장선웅
      hw 1 - Exercise 3
  */
  
  
  def iter(((n, f)), x) = { if (n <= 0) x else iter(n - 1, f, f(x)) }
  
  
}
