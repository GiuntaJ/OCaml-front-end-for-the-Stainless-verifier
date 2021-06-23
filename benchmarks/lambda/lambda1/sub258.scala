import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub258 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkWithTwoVariables(((regions, stations))) = {
    stations match {
      case AREA(reg, rest) => { checkWithTwoVariables(reg :: regions, rest) }
      case CONNECT(rest1, rest2) => {
        checkWithTwoVariables(regions, rest1) &&
        checkWithTwoVariables(regions, rest2)
      }
      case STATION(stat) => { if (regions.contains(stat)) true else false }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    m match {
      case AREA(region, restMetro) => {
        checkWithTwoVariables(List(region), restMetro)
      }
      case CONNECT(metro1, metro2) => { checkMetro(metro1) && checkMetro(metro2)
      }
      case _ => { false }
    }
  }
}
