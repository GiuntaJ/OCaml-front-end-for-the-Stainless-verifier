import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub382 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkhelper(((x, l))) = {
    x match {
      case STATION(y) => { l.contains(y) }
      case AREA(y, z) => { checkhelper(z, y :: l) }
      case CONNECT(y, z) => { checkhelper(y, l) && checkhelper(z, l) }
    }
  }
  
  def checkMetro(x: Metro): Boolean = { checkhelper(x, Nil()) }
}