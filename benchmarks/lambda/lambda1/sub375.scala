import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub375 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  /* let rec checkConnect1(input1,inputlist) = match input1 with
    |STATION a -> if List.mem a inputlist then true else false
    |AREA(a,b) -> checkArea(b, (addtolist inputlist a))
    |CONNECT(a,b) */
  
  /* let rec checkConnect2(input2,inputlist) = match input2 with  */
  
  def addtolist[A](inputlist: List[A], element: A): List[A] = {
    List(element) ++(inputlist)
  }
  
  def checkArea(((input, inputarray))) = {
    input match {
      case STATION(b) => { if (inputarray.contains(b)) true else false }
      case AREA(a, b) => { checkArea(b, addtolist(inputarray, a)) }
      case CONNECT(a, b) => {
        
          if (
            checkArea(a, inputarray) && checkArea(b, inputarray)
          ) {
            true 
          } else {
            false
          }
      }
    }
  }
  
  def checkMetroHelp(input: Metro, inputarray: List[Name]): Boolean = {
    input match {
      case AREA(a, b) => { checkArea(b, addtolist(inputarray, a)) }
      case STATION(x) => { false }
      case CONNECT(a, b) => {
        
          if (
            checkArea(a, inputarray) && checkArea(b, inputarray)
          ) {
            true 
          } else {
            false
          }
      }
    }
  }
  
  def checkMetro(input: Metro): Boolean = { checkMetroHelp(input, Nil()) }
  
  
  /* let _ =
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
    test_case(10, false == checkMetro(STATION "a")) */
}