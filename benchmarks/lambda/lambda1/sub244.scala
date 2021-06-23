import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub244 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetroProc(met: Metro, l: List[Name]): Boolean = {
    met match {
      case STATION(n) => { l.contains(n) }
      case AREA(n, m) => { checkMetroProc(m, l ++ List(n)) }
      case CONNECT(m1, m2) => { checkMetroProc(m1, l) && checkMetroProc(m2, l) }
    }
  }
  
  def checkMetro(met: Metro): Boolean = {
    met match {
      case STATION(n) => { false }
      case AREA(n, m) => { checkMetroProc(m, List(n)) }
      case CONNECT(m1, m2) => {
        checkMetroProc(m1, Nil()) && checkMetroProc(m2, Nil())
      }
    }
  }
}