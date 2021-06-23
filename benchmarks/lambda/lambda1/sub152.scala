import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub152 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  
  def subcheckMetro(m: Metro, l: List[Name]): Boolean = {
    m match {
      case STATION(a) => { l.contains(a) }
      case AREA(a, b) => { subcheckMetro(b, a :: l) }
      case CONNECT(a, b) => { subcheckMetro(a, l) && subcheckMetro(b, l) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { subcheckMetro(m, Nil()) }
}