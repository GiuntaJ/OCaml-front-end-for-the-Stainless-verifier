import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub455 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetroHelp(((x: Metro, l: List[A]))): Boolean = {
    x match {
      case STATION(s) => { l.contains(s) }
      case CONNECT(metro1, metro2) => {
        checkMetroHelp(metro1, l) && checkMetroHelp(metro2, l)
      }
      case AREA(s, metro) => { checkMetroHelp(metro, s :: l) }
    }
  }
  
  def checkMetro(x: Metro): Boolean = { checkMetroHelp(x, Nil()) }
}