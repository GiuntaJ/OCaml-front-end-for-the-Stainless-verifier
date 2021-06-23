import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub399 {
  /*컴퓨터공학부/2011-11729/안진우/2-4*/
  
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro_h(((x: Metro, l: List[Name]))): Boolean = {
    x match {
      case STATION(n) => { if (l.contains(n)) true else false }
      case AREA(n, m) => { checkMetro_h(m, List(n) ++(l)) }
      case CONNECT(m1, m2) => { checkMetro_h(m1, l) && checkMetro_h(m2, l) }
    }
  }
  
  def checkMetro(x: Metro): Boolean = { checkMetro_h(x, Nil()) }
}