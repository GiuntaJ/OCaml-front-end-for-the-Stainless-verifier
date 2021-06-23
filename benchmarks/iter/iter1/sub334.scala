import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub334 {
  /* f : 'a -> 'a */
  /*
  let rec iter_naive (n, f) =
    if (n<=0) then (fun x -> x)
    else (fun x -> f (iter_naive(n-1,f) x))
  ;;
  **/
  def iter(((n, f))) = {
    val _2 = {
      def aux(((n, f, x, acc))) = {
        if (n <= 0) acc else aux(n - 1, f, x, f(acc))
      }
      ( (x) => { aux(n, f, x, x) } )
    }
  }
  
  
  /* Testcases */
  /*
  let _ = print_int (iter(10, function x -> 2+x) 0) /* 20 */
  let _ = print_endline ""
  let _ = print_int (iter(10, function x -> 2*x) 2) /* 2^11 */
  let _ = print_endline ""
  let _ = print_int (iter(10, function x -> 2*x) 4) /* 2^12 */
  let _ = print_endline ""
  let _ = print_int (iter(0, function x -> 2*x) 5) /* 5 */
  let _ = print_endline ""
  let _ = print_int (iter(-1, function x -> 2*x) 5) /* 5 */
  let _ = print_endline ""
  */
}