import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub441 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkArea(a: (List[Name], Metro)): Boolean = {
    a match {
      case (Nil(), STATION(n)) => { false }
      case (l, STATION(n)) => { l.contains(n) }
      case (l, AREA(n, m)) => { checkArea(n :: l, m) }
      case (l, CONNECT(m1, m2)) => { checkArea(l, m1) && checkArea(l, m2) }
    }
  } 
  
  def checkMetro: Metro => Boolean = (
    (ch) =>
      {
        ch match {
          case STATION(n) => { false }
          case CONNECT(m1, m2) => { checkMetro(m1) && checkMetro(m2) }
          case AREA(n, m) => { checkArea(List(n), m) }
        }
    }
  )
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