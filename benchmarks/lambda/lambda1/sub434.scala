import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub434 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def retname(met: Metro): List[String] = {
    met match {
      case STATION(str) => { List(str) }
      case AREA(str, metro) => { retname(metro).filter(( (x) => { x != str } ))
      }
      case CONNECT(met1, met2) => { retname(met1) ++(retname(met2)) }
    }
  }
  
  def checkMetro(met: Metro): Boolean = {
    met match {
      case STATION(name) => { false }
      case AREA(name, metro) => { if (retname(met) eq Nil()) true else false }
      case CONNECT(me1, me2) => { checkMetro(me1) && checkMetro(me2) }
    }
  }
  
  
  
  /*
  /*test new*/
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
  
  /*test old*/
  let _= 
  let print_bool x = print_endline (string_of_bool x) in 
  
  let a81 = checkMetro (AREA("a", STATION "a")) in 
  let a82 = checkMetro (AREA("a", AREA("a", STATION "a"))) in 
  let a83 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))) in 
  let a84 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))) in 
  let a85 = checkMetro (AREA("a", STATION "b")) in 
  let a86 = checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))) in 
  let a87 = checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))) in 
  
  print_bool(false = checkMetro ( STATION "a")); 
  print_bool(true = checkMetro ( CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "a")))))); 
  print_bool(false = checkMetro ( CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "c")))))); 
  print_bool(true = a81); 
  print_bool(true = a82); 
  print_bool(true = a83); 
  print_bool(true = a84); 
  print_bool(false = a85); 
  print_bool(false = a86); 
  print_bool(false = a87) 
  ;;*/
}