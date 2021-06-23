import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub464 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetroHelper(((m: Metro, l: List[String]))): Boolean = {
    m match {
      case STATION(s0) => { l.contains(s0) }
      case AREA(s0, m0) => { checkMetroHelper(m0, s0 :: l) }
      case CONNECT(m1, m2) => {
        checkMetroHelper(m1, l) && checkMetroHelper(m2, l)
      }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkMetroHelper(m, Nil()) }
}