import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub14 {
  /*
   * Student no. : 2009-20769
   * Name        : Kim, Seongjun
   */
  sealed abstract class Metro {}
  case class STATION(param0: String) extends Metro {}
  case class AREA(param0: String,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def checkMetro(m: Metro): Boolean = {
    val _2 = {
      def find_name(name, nlist) = { nlist.exists(( (n) => { n == name } )) }
      val _3 = {
        def check(m, nlist) = {
          m match {
            case STATION(n) => { find_name(n, nlist) }
            case AREA(n, mm) => { check(mm, n :: nlist) }
            case CONNECT(m1, m2) => { check(m1, nlist) && check(m2, nlist) }
          }
        }
        check(m, Nil())
      }
    }
  }
  
  /* TEST
  open OUnit;;
  
  let test_checkMetro _ =
      assert_equal true (checkMetro (AREA("a", STATION "a")));
      assert_equal true (checkMetro (AREA("a", AREA("a", STATION "a"))));
      assert_equal true (checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))));
      assert_equal true (checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))));
  
      assert_equal false (checkMetro (AREA("a", STATION "b")));
      assert_equal false (checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))));
      assert_equal false (checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))))
  
  let suite = "Test Excercise" >::: ["test of checkMetro" >:: test_checkMetro]
  
  let _ = run_test_tt_main suite
  */
}