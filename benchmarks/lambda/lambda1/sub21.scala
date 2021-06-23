import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub21 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(met: Metro): Boolean = { checkMetro2(met, Nil()) }
  def checkMetro2(met, strs) = {
    met match {
      case STATION(n) => { if (strs.contains(n)) true else false }
      case AREA(n, m) => { checkMetro2(m, n :: strs) }
      case CONNECT(m1, m2) => { checkMetro2(m1, strs) && checkMetro2(m2, strs) }
    }
  }
  	
}