import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub334 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro2: (Metro, List[String]) => Boolean = {
    case (m, s) =>
      {
        m match {
          case STATION(k) => { if (s.contains(k)) true else false }
          case AREA(k, q) => { checkMetro2(q, k :: s) }
          case CONNECT(k, q) => { checkMetro2(k, s) && checkMetro2(q, s) }
        }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkMetro2(m, Nil()) }
  
}
