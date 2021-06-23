import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub46 {
  /* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#1-7 */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetroPre(((m, l))) = {
    m match {
      case STATION(a) => { l.contains(a) }
      case AREA(a, b) => { checkMetroPre(b, a :: l) }
      case CONNECT(a, b) => { checkMetroPre(a, l) && checkMetroPre(b, l) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkMetroPre(m, Nil()) }
  
  /*
  /* test code */
  let _ = print_endline (string_of_bool (checkMetroPre (AREA("a", STATION "a"), [])));
  	print_endline (string_of_bool (checkMetroPre (AREA("a", AREA("a", STATION "a")), [])));
  	print_endline (string_of_bool (checkMetroPre (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))), [])));
  	print_endline (string_of_bool (checkMetroPre (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))), [])));
  	print_endline (string_of_bool (checkMetroPre (AREA("a", STATION "b"), [])));
  	print_endline (string_of_bool (checkMetroPre (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))), [])));
  	print_endline (string_of_bool (checkMetroPre (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))), [])))
  */
}