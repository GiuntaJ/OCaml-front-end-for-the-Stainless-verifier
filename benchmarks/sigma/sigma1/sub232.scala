import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub232 {
  /*
   * 컴퓨터공학부 2009-11690 김찬민
   * Homework 1 Exercise 1  */
  def sigma(((a: Int63, b: Int63, f: Int63 => Int63))): Int63 = {
    if (a > b) 0 else f(a) + sigma(a + 1, b, f)
  }
}