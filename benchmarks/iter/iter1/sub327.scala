import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub327 {
  /* Mechanical & Aerospace Eng./2013-11706/Kang Injae/1-3.ml */
  
  def iter[A](((n: Int63, f: A => A)), input: A): A = {
    if (n == 0) input else f(iter(n - 1, f, input))
  }
  
  /*
  let fcn = fun x -> x ^ x
  
  let _ = (print_string (iter (3, fcn) "Hello ")); print_newline()
  */
}