import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub352 {
  /*
    CSE/2015-21233/김종권
    Homework 2-3
  */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro_0(m: Metro, l: List[Name]): Boolean = {
    m match {
      case STATION(name) => { l.exists(( (x) => { x == name } )) }
      case AREA(name, m_0) => { checkMetro_0(m_0, name :: l) }
      case CONNECT(m1, m2) => { checkMetro_0(m1, l) && checkMetro_0(m2, l) }
    }
  }
        
  def checkMetro(m: Metro): Boolean = { checkMetro_0(m, Nil()) }
}
