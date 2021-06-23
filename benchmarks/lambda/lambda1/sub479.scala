import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub479 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetroList(((m: Metro, l: List[Name]))): Boolean = {
    m match {
      case STATION(n1) => { if (l.contains(n1)) true else false }
      case AREA(n1, m1) => { checkMetroList(m1, n1 :: l) }
      case CONNECT(m1, m2) => { checkMetroList(m1, l) && checkMetroList(m2, l) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkMetroList(m, Nil()) }
}