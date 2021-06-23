import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub463 {
  
  /* EXERCISE 4 */ 
  
  type Name = String
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def find_namelist: (Name, List[Name]) => Boolean = {
    case (input, l) =>
      {
        l match {
          case Nil() => { false }
          case Cons(h, t) => { if (h == input) true else find_namelist(input, t)
          }
        }
    }
  }
  
  def checkMetroProbe: (Metro, List[Name]) => Boolean = {
    case (input, nlist) =>
      {
        input match {
          case STATION(station_name) => { find_namelist(station_name, nlist) }
          case AREA(area_name, subinput) => {
            checkMetroProbe(subinput, area_name :: nlist)
          }
          case CONNECT(subinput1, subinput2) => {
            checkMetroProbe(subinput1, nlist) &&
            checkMetroProbe(subinput2, nlist)
          }
        }
    }
  }
  
  
  val checkMetro: Metro => Boolean = ( (input) => { checkMetroProbe(input, Nil()) } )
  
  /*
  let _ = 
  let print_bool x = 
  print_endline (string_of_bool x) in 
  print_bool(true = (find_namelist "a" ("b"::"a"::"c"::[])))
  */	
  /*
  let _ = 
  let print_bool x = 
  print_endline (string_of_bool x) in 
  print_bool(true = checkMetro ( CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "a"))))));
  print_bool(false = checkMetro ( CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "c")))))); 
  print_bool(true = checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))));
  print_bool(true = checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))));
  print_bool(true = checkMetro(AREA("a", AREA("a", STATION "a"))));
  print_bool(true = checkMetro(AREA("a", STATION "a")));
  print_bool(false = checkMetro(AREA("a", STATION "b")));
  print_bool(false = checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))));
  print_bool(false = checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))));
  */
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
  test_case(10, false == checkMetro(STATION "a"));
  */
}