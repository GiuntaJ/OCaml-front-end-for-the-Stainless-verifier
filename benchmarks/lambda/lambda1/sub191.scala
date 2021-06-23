import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub191 {
  type Name = String
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def checkMetro_list(x: Metro, l: List[Name]): Boolean = {
    x match {
      case STATION(n) => { l.exists(( (m) => { n == m } )) }
      case AREA(n, m) => { checkMetro_list(m, l ++(List(n))) }
      case CONNECT(m1, m2) => { checkMetro_list(m1, l) && checkMetro_list(m2, l)
      }
    }
  }
  
  def checkMetro(x: Metro): Boolean = { checkMetro_list(x, Nil()) } 
}