import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub194 {
  /* 2014 - 18474 kim ju hyeon  */
  /* 2017 fall PL 1_3 */
  def iter(((n, f))) = {
    if (n <= 0) ( (x) => { x } ) else ( (x) => { iter(n - 1, f, f(x)) } )
  }
      
  /* 
  let _ = 
  let print_int x = 
  print_endline (string_of_int x) in 
    print_int ( iter (0, fun x -> 2+x) 4);
    print_int ( iter (3, fun x -> x * 2) 2);
    
   */
}