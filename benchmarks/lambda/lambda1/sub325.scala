import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub325 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def myCheck(m: Metro, l: List[Name]): Boolean = {
    m match {
      case STATION(n) => { l.contains(n) }
      case AREA(n, mm) => {
        if (l.contains(n)) myCheck(mm, l) else myCheck(mm, n :: l)
      }
      case CONNECT(mm1, mm2) => { myCheck(mm1, l) && myCheck(mm2, l) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { myCheck(m, Nil()) }
}