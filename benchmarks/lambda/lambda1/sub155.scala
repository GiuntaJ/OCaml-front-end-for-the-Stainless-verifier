import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub155 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def check_with_list(m: Metro, l: List[Name]): Boolean = {
    m match {
      case STATION(str) => { l.contains(str) }
      case CONNECT(a, b) => { check_with_list(a, l) && check_with_list(b, l) }
      case AREA(str, met) => { check_with_list(met, l ++(List(str))) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { check_with_list(m, Nil()) }
}