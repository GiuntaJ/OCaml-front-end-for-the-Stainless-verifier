import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_sigma_sigma1_sub13 {
  /*
   * Student no. : 2009-20769
   * Name        : Kim, Seongjun
   */
  
  def sigma(((a, b, f))) = { if (a > b) 0 else f(a) + sigma(a + 1, b, f) }
  
  /* TEST 
  open OUnit;;
  
  let test_sigma _ =
      assert_equal 0 (sigma (1, 0, (fun n -> n)));
      assert_equal 55 (sigma (1, 10, (fun n -> n)));
      assert_equal 30 (sigma (1, 4, (fun n-> n*n)));
      assert_equal 1 (sigma (1, 1, (fun n -> n)))
  
  let suite = "Test Excercise1" >::: ["test of sigma" >:: test_sigma]
  
  let _ = run_test_tt_main suite
   */
}