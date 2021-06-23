import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub118 {
  /* 2010-11753 snucse Taekmin Kim */
  /* HW 1-3 */
  
  def iter: ((Int63, (A => A)), A) => A = {
    case (n, fn) =>
      { if (n eq 0) ( (x) => { x } ) else ( (x) => { iter(n - 1, fn, fn(x)) } )
    }
  }
  
  /*
  let _ = print_endline(string_of_int(iter(3, fun x -> 2 + x) 0))
  */
}