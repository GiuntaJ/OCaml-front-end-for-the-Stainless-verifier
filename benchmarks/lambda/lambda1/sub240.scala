import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub240 {
  type Name = String
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def check(m: Metro, l: List[Name]): Boolean = {
    m match {
      case STATION(id) => { l.contains(id) }
      case CONNECT(m1, m2) => { check(m1, l) && check(m2, l) }
      case AREA(id, met) => { check(met, id :: l) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    m match {
      case STATION(id) => { false }
      case CONNECT(m1, m2) => { checkMetro(m1) && checkMetro(m2) }
      case AREA(id, met) => { check(met, List(id)) }
    }
  }
}