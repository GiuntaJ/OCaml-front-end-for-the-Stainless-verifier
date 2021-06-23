import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub439 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def withlist(m: Metro, nl: List[String]): Boolean = {
    m match {
      case AREA(n, me) => { withlist(me, n :: nl) }
      case CONNECT(m1, m2) => { withlist(m1, nl) && withlist(m2, nl) }
      case STATION(n) => { nl.contains(n) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { withlist(m, Nil()) } 
}