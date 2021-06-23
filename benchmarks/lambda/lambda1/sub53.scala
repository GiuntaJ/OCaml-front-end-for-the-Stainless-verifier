import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub53 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkMetro(metro: Metro): Boolean = {
    metro match {
      case STATION(name) => { false }
      case AREA(name1, STATION(name2)) => { name1 == name2 }
      case AREA(name1, AREA(name2, CONNECT(metro1, metro2))) => {
        checkMetro(AREA(name1, metro1)) && checkMetro(AREA(name2, metro2)) ||
        checkMetro(AREA(name1, metro2)) && checkMetro(AREA(name2, metro1))
      }
      case AREA(name1, AREA(name2, metro)) => {
        checkMetro(AREA(name1, metro)) || checkMetro(AREA(name2, metro))
      }
      case AREA(name, CONNECT(metro1, metro2)) => {
        checkMetro(AREA(name, metro1)) && checkMetro(AREA(name, metro2))
      }
      case CONNECT(metro1, metro2) => { checkMetro(metro1) && checkMetro(metro2)
      }
    }
  }
}
