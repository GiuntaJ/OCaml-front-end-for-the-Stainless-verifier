import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter2_sub7 {
  /* problem 3*/
  def fun_add(f1, f2, x) = { f2(f1(x)) }
  def fun_adds(n, f) = {
    if (n == 0) ( (x) => { x } ) else fun_add(f, fun_adds(n - 1, f))
  } 
  
  def iter: ((Int63, (Int63 => Int63)), Int63) => Int63 = {
    case (n, f) => { fun_adds(n, f) }
  }
}