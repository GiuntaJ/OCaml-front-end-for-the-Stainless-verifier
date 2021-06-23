import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub224 {
  /*Computer Engineering 2015-12683 Kim Jaein Exercise 1-3*/
  def iter(((n, f))) = {
    
      if (
        n <= 0
      ) {
        ( (x) => { x } ) 
      } else if (
        n eq 1
      ) {
        ( (x) => { f(x) } ) 
      } else {
        ( (x) => { f(iter(n - 1, f, x)) } )
      }
  }
  /*
  let twice x = x * x
  
  let () = print_endline (string_of_int (iter (2, twice) 3))
  */
}