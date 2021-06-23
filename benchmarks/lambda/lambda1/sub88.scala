import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub88 {
  type Name = String
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def checkName(((n, mtr))) = {
    mtr match {
      case STATION(s) => { if (n == s) true else false }
      case AREA(n1, mtr1) => { checkName(n1, mtr1) }
      case CONNECT(m1, m2) => {
        (m1, m2) match {
          case (_, AREA(nm1, mt1)) => { checkName(n, mt1) }
          case (AREA(nm2, mt2), _) => { checkName(n, mt2) }
          case (_, _) => { checkName(n, m1) || checkName(n, m2) }
        }
      }
    }
  }
  		
  
  def checkMetro(m: Metro): Boolean = {
    m match {
      case AREA(n, mtr) => { checkName(n, mtr) }
      case _ => { false }
    }
  }
}