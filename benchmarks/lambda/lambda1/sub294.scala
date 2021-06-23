import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub294 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      def checkMetroWithNamespace(((sub_metro, namespace))) = {
        sub_metro match {
          case STATION(n) => { namespace.contains(n) }
          case AREA(n, m) => { checkMetroWithNamespace(m, n :: namespace) }
          case CONNECT(m1, m2) => {
            checkMetroWithNamespace(m1, namespace) &&
            checkMetroWithNamespace(m2, namespace)
          }
        }
      }
      checkMetroWithNamespace(metro, Nil())
    }
  }
  
  /* Test Case */
  /*
  let _ =
    let print_bool x =
      print_endline (string_of_bool x)
    in
  
    print_bool (true = checkMetro ( CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "a"))))));
    print_bool (false = checkMetro ( CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "c"))))));
    print_bool (true = checkMetro ( AREA ("a", STATION "a")));
    print_bool (true = checkMetro ( AREA("a", AREA("a", STATION "a"))));
    print_bool (true = checkMetro ( AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))));
    print_bool (true = checkMetro ( AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))));
    print_bool (false = checkMetro ( AREA("a", STATION "b")));
    print_bool (false = checkMetro ( AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))));
    print_bool (false = checkMetro ( AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))));
    */
}