import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub512 {
  /* sigma : int * int * (int -> int) -> int */
  /*
  let rec sigma_naive (a,b,f) =
    if (a>b) then 0
    else f(a) + sigma_naive(a+1, b, f)
  **/
  
  def sigma(((a, b, f))) = {
    val _2 = {
      def aux(((a, b, f, acc))) = {
        if (a > b) acc else aux(a + 1, b, f, acc + f(a))
      }
      aux(a, b, f, 0)
    }
  }
  
  
  /*
  print_int (sigma(1,10, fun x->x*x))
  **/
}