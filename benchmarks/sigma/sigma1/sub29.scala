import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub29 {
  sealed case class Error(param0: String) extends Exception {}
  def sigma(((a, b, f))) = {
    
      if (
        a > b
      ) {
        assert(false, "Error with a > b ") 
      } else if (
        a == b
      ) {
        f(a) 
      } else {
        f(a) + sigma(a + 1, b, f)
      }
  }
  
  def f1(n) = { n }
  def f2(n) = { n * n }
  /*
  let _ = 
    print_int (sigma (1,100,f1));
    print_newline();
    print_int (sigma (1,100,f2));
    print_newline();
    print_int (sigma (1,1,f1));
    print_newline();
    print_int (sigma (100,1,f1));
    print_newline();*/
}