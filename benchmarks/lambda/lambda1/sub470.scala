import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub470 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (m) =>
      {
        val _4 = {
          def isNameInList: (Name, List[Name]) => Boolean = {
            case (n, nl) =>
              {
                nl match {
                  case Nil() => { false }
                  case Cons(nh, nt) => { nh == n || isNameInList(n, nt) }
                }
            }
          }
          val _5 = {
            def checkMetroWithNameSpace: (Metro, List[Name]) => Boolean = {
              case (m, nl) =>
                {
                  m match {
                    case STATION(id) => { isNameInList(id, nl) }
                    case CONNECT(a, b) => {
                      checkMetroWithNameSpace(a, nl) &&
                      checkMetroWithNameSpace(b, nl)
                    }
                    case AREA(id, mt) => { checkMetroWithNameSpace(mt, id :: nl)
                    }
                  }
              }
            }
            checkMetroWithNameSpace(m, Nil())
          }
        }
    }
  )
  
  /* TEST */
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