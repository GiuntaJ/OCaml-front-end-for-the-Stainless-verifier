import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub270 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String 
  
  def checkMetroG(lst: List[Name], met: Metro): Boolean = {
    met match {
      case AREA(n, m) => { checkMetroG(n :: lst, m) }
      case CONNECT(m1, m2) => { checkMetroG(lst, m1) && checkMetroG(lst, m2) }
      case STATION(t) => { if (lst.contains(t)) true else false }
    }
  }
  
  val checkMetro: Metro => Boolean = checkMetroG(Nil())	
}