import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub196 {
  def iter(((n, f))) = {
    if (n eq 0) ( (x) => { x } ) else ( (x) => { f(iter(n - 1, f, x)) } )
  }
  /*
  let test_f x=
    2+x
  
  let _ = print_int ( (iter (0, test_f) 8)) ; print_endline("")
  let _ = print_int ( (iter (1, test_f) 8)); print_endline("")
  let _ = print_int ( (iter (5, test_f) 8)); print_endline("")
  */
}