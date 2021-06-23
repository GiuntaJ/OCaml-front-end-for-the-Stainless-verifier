import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub130 {
  /* 2011-10915 / 생명과학부 / 신지민 / Homework 1-3 */
  
  def iter: ((Int63, (A => A)), A) => A = {
    case ((n, f), a) => { if (n eq 0) a else f(iter(n - 1, f, a)) }
  }
  
  
  /*
  let k = iter(3, function x -> x^x) "x"
  
  let _= print_endline(k)
  
  */
}