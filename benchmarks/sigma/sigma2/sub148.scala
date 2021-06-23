import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub148 {
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) => { if (a > b) 0 else f(a) + sigma(f, a + 1, b) }
  }
  
  /* print_int sigma (fun x -> x) 1 10;; */
  /* print_int sigma (fun x -> x*x) 1 7;; */
}
