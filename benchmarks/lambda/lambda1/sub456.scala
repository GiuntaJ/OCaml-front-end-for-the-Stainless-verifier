import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub456 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def make_list(((n: Name, l: List[Name]))): List[Name] = { n :: l }
  
  def checkM(((m: Metro, l: List[Name]))): Boolean = {
    m match {
      case AREA(n, ml) => { checkM(ml, make_list(n, l)) }
      case STATION(n) => { l.contains(n) }
      case CONNECT(m1, m2) => { checkM(m1, l) && checkM(m2, l) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkM(m, Nil()) }
  
  /*-------------------test case----------------*/
  /*
  let _ = 
     let test_case : int * bool -> unit = fun (n, x) -> 
  	      print_endline ("Case " ^ string_of_int(n) ^ " : " ^ string_of_bool(x)) in 
  				   test_case(1, true == checkMetro(AREA("a", STATION "a"))); 
  					    test_case(2, true == checkMetro(AREA("a", AREA("a", STATION "a")))); 
  							   test_case(3, true == checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))))); 
  								    test_case(4, true == checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))))); 
  										   test_case(5, false == checkMetro(AREA("a", STATION "b"))); 
  											    test_case(6, false == checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))); 
  													   test_case(7, false == checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))); 
  														    test_case(8, true == checkMetro(CONNECT(AREA("a", STATION "a"), AREA("b", AREA("a", CONNECT(STATION "b", STATION "a")))))); 
  																   test_case(9, false == checkMetro(CONNECT(AREA("c", STATION "c"), AREA("b", AREA("a", CONNECT(STATION "b", STATION "c")))))); 
  																	    test_case(10, false == checkMetro(STATION "a"))
  */
}