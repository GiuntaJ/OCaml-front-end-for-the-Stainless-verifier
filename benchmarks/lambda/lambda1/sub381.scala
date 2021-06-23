import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub381 {
  /*2016-11690*/
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  val checkMetro: Metro => Boolean = (
    (metr) =>
      {
        val _4 = {
          def check: (Metro, List[A]) => Boolean = {
            case (inputMetro, areaList) =>
              {
                inputMetro match {
                  case STATION(name) => { areaList.contains(name) }
                  case AREA(name, met) => { check(met, name :: areaList) }
                  case CONNECT(met1, met2) => {
                    if (check(met1, areaList)) check(met2, areaList) else false
                  }
                }
            }
          }
          check(metr, Nil())
        }
    }
  )
  
  /*test code 
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
  ;;
  
  */
}