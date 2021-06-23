import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub473 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(m: Metro): Boolean = {
    m match {
      case STATION(n) => { true }
      case CONNECT(m1, m2) => { checkMetro(m1) && checkMetro(m2) }
      case AREA(n, m) => {
        m match {
          case STATION(n2) => { if (n == n2) true else false }
          case _ => { checkMetro(m) }
        }
      }
    }
  }
  
}
