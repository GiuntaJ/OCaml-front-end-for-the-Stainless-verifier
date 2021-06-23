import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub292 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkMetro_list(m: Metro, a_list: List[Name]): Boolean = {
    m match {
      case STATION(s) => { a_list.contains(s) }
      case AREA(a, m1) => { checkMetro_list(m1, a :: a_list) }
      case CONNECT(m2, m3) => {
        checkMetro_list(m2, a_list) && checkMetro_list(m3, a_list)
      }
    }
  }
  
  
  def checkMetro(m: Metro): Boolean = { checkMetro_list(m, Nil()) }
}
