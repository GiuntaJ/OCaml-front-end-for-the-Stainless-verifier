import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma2_sub196 {
  def sigma: (Int63 => Int63, Int63, Int63) => Int63 = {
    case (f, a, b) => { 0 }
  } 
  /*match f with
   (fun x -> x) -> (if a <= b then a + (sigma f (a+1) b) else 0)
  |(fun x -> x*x) -> (if a <= b then a * (sigma f (a+1) b) else 0)
  | 0;;*/
}