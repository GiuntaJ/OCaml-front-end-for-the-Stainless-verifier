import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub351 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def station_list(x: Metro): List[Name] = {
    x match {
      case STATION(n) => { List(n) }
      case AREA(n, m) => { station_list(m) }
      case CONNECT(m1, m2) => { station_list(m1) ++(station_list(m2)) }
    }
  }
  
  def checkMetro(x: Metro): Boolean = {
    x match {
      case STATION(n) => { false }
      case AREA(n, m) => { station_list(m).contains(n) }
      case CONNECT(m1, m2) => { checkMetro(m1) && checkMetro(m2) }
    }
  }
}