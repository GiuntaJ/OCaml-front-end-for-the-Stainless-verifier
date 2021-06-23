import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub229 {
  def iter(((n, f)), a) = {
    n match {
      case 0 => { a }
      case _ => { iter(n - 1, f, f(a)) }
    }
  }
  
  
  /*let a31 = iter (3, function x -> 2+x) 0
  let a32 = iter (0, function x -> 2*x) 4
  let a33 = iter (11, function x -> 2*x+1) 7 
  
  let _ = print_endline(string_of_int a31)
  let _ = print_endline(string_of_int a32)
  let _ = print_endline(string_of_int a33)*/
}