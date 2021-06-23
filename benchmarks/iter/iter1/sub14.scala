import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_iter_iter1_sub14 {
  /*
   * Student no. : 2009-20769
   * Name        : Kim, Seongjun
   */
  sealed case class Error(param0: String) extends Exception {}
  
  def iter(((n, f))) = {
    val _2 = {
      def fun_composit(f1, f2, x) = { f1(f2(x)) }
      val _3 = {
        def identity(x) = { x }
        
          if (
            n < 0
          ) {
            assert(false, "Error with # of iter is less than 0 ") 
          } else if (
            n == 0
          ) {
            identity 
          } else {
            fun_composit(f, iter(n - 1, f))
          }
      }
    }
  }
  
  /*
  ;;
  iter ((-1), (fun n -> n * n)) 1234;;    /* Exception */
  iter (0, (fun n -> n * n)) 1234;;       /* 1234 */
  iter (2, (fun n -> n * n)) 2;;          /* 16 */
  iter (100, ((+) 2)) 0;;                 /* 200 */
  iter (10000, ((+) 2)) 0;;               /* 20000 */
  iter (2, List.tl) [1;2;3;4];;           /* [3; 4] */
  */
}