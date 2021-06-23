import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub392 {
  /*
   * 2017 - 09 - 22
   * PL Homework 2-4
   * Joonmo Yang
  */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (mt) =>
      {
        val _4 = {
          def checkMetro_sub: (Metro, List[Name]) => Boolean = {
            case (m, nlist) =>
              {
                m match {
                  case STATION(s) => { nlist.exists(( (x) => { x == s } )) }
                  case AREA(nm, mtr) => {
                    
                      if (
                        nlist.exists(( (x) => { x == nm } ))
                      ) {
                        checkMetro_sub(mtr, nlist) 
                      } else {
                        checkMetro_sub(mtr, nm :: nlist)
                      }
                  }
                  case CONNECT(mtr1, mtr2) => {
                    checkMetro_sub(mtr1, nlist) && checkMetro_sub(mtr2, nlist)
                  }
                }
            }
          }
          checkMetro_sub(mt, Nil())
        }
    }
  )
  
  /* test cases
  let a1 = AREA("a", STATION "a")
  let a2 = AREA("a", AREA("a", STATION "a"))
  let a3 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))
  let a4 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))
  
  let b1 = checkMetro a1
  let b2 = checkMetro a2
  let b3 = checkMetro a3
  let b4 = checkMetro a4
  
  let a5 = AREA("a", STATION "b")
  let a6 = AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))
  let a7 = AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))
  
  let b5 = checkMetro a5
  let b6 = checkMetro a6
  let b7 = checkMetro a7
  
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