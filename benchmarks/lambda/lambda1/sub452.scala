import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub452 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkmetrolist(((m: Metro, l: List[String]))): Boolean = {
    m match {
      case STATION(x) => { l.contains(x) }
      case AREA(a, m_0) => { checkmetrolist(m_0, l ++ List(a)) }
      case CONNECT(x, y) => { checkmetrolist(x, l) && checkmetrolist(y, l) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkmetrolist(m, Nil()) }
}