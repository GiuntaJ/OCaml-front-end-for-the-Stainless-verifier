import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub449 {
  /* 컴퓨터공학부 2013-11425 이창영 hw2_4 */
  type Name = String
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def checkMetro2(m: Metro, l: List[String]): Boolean = {
    m match {
      case STATION(a) => { if (l.contains(a)) true else false }
      case AREA(a, b) => { checkMetro2(b, l ++ List(a)) }
      case CONNECT(a, b) => { checkMetro2(a, l) && checkMetro2(b, l) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkMetro2(m, Nil()) }
}