import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub308 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetroLi: (Metro, List[Name]) => Boolean = {
    case (m, li) =>
      {
        m match {
          case STATION(n) => { if (li.contains(n)) true else false }
          case AREA(n, mm) => {
            val _2 = {
              val childList: List[Name] = n :: li
              checkMetroLi(mm, childList)
            }
          }
          case CONNECT(mm1, mm2) => {
            checkMetroLi(mm1, li) && checkMetroLi(mm2, li)
          }
        }
    }
  }
  
  val checkMetro: Metro => Boolean = ( (m) => { checkMetroLi(m, Nil()) } )
  
  /*
  let testf = fun x ->
      if( checkMetro(x)) then (print_endline "true") else (print_endline "false");;
  
  testf(AREA("a", STATION "a"));
  testf(AREA("a", AREA("a", STATION "a")));
  testf(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))));
  testf(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))));
  
  testf(AREA("a", STATION "b"));
  testf(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))));
  testf(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))));
  */
  
}
