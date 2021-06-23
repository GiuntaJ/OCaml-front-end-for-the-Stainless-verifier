import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub387 {
  def sum_list(fn) = {
    (
      x =>
        x match {
          case Nil() => { 0 }
          case Cons(hd, tl) => { fn(hd) + sum_list(fn, tl) }
        }
    )
  }
  
  def make_list(a: Int63, b: Int63): List[Int63] = {
    if (a <= b) a :: make_list(a + 1, b) else Nil()
  }
  
  def sigma(((a: Int63, b: Int63, fn: Int63 => Int63))) = {
    sum_list(fn, make_list(a, b))
  }
}