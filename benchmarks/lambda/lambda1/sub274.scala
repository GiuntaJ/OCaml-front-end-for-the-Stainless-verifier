import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub274 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetroHelper(areaList: List[Name], metro: Metro): Boolean = {
    metro match {
      case STATION(name) => { if (areaList.contains(name)) true else false }
      case AREA(name, metro) => { checkMetroHelper(name :: areaList, metro) }
      case CONNECT(metro1, metro2) => {
        checkMetroHelper(areaList, metro1) && checkMetroHelper(areaList, metro2)
      }
    }
  }
  
  def checkMetro(metro: Metro): Boolean = { checkMetroHelper(Nil(), metro) }
  
  /*
  let print_bool x = print_endline (string_of_bool x);;
  
  print_bool(checkMetro ( CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "a"))))));;
  print_bool(checkMetro ( CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "c"))))));;
  */
}