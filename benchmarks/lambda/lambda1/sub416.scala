import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub416 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro: Metro => Boolean = (
    (metro) =>
      {
        metro match {
          case STATION(s) => { false }
          case AREA(n, m) => {
            m match {
              case STATION(s) => { if (n == s) true else false }
              case AREA(n, m) => { checkMetro(AREA(n, m)) }
              case CONNECT(m1, m2) => {
                checkMetro(AREA(n, m1)) || checkMetro(AREA(n, m2))
              }
            }
          }
          case CONNECT(m1, m2) => { false }
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