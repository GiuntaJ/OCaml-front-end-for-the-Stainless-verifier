import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub14 {
  
  /*
   * Student no. : 2009-20769
   * Name        : Kim, Seongjun
   */
  
  sealed case class Error(param0: String) extends Exception {}
  
  def sigma(((a, b, f))) = {
    val _2 = {
      def s(a, b, f) = { if (a > b) 0 else f(a) + s(a + 1, b, f) }
      
        if (
          a > b
        ) {
          assert(false, "Error with Initial value is grater than final value ") 
        } else {
          s(a, b, f)
        }
    }
  }
  
  /*
  ;;
  sigma (1, 0, (fun n -> n));;     /* Exception */
  sigma (1, 10, (fun n -> n));;    /* 55 */
  sigma (1, 4, (fun n-> n*n));;    /* 30 */
  sigma (1, 1, (fun n -> n));;     /* 1 */
  */
}