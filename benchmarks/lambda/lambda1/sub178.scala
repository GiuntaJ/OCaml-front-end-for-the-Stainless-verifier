import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub178 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkArea(n: List[Name], e: Metro): Boolean = {
    e match {
      case AREA(a, b) => { checkArea(n ++ List(a), b) }
      case CONNECT(a, b) => { checkArea(n, a) && checkArea(n, b) }
      case STATION(a) => { n.exists(( (x) => { x == a } )) }
    }
  }
  
  def checkMetro(e: Metro): Boolean = {
    e match {
      case AREA(a, b) => { checkArea(List(a), b) }
      case CONNECT(a, b) => { checkMetro(a) && checkMetro(b) }
      case STATION(n) => { true }
    }
  } /* no works - default value */
}