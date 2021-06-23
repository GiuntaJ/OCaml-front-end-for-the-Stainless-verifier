import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub243 {
  /* hw 2-1 */
  /* 2012-11269 DongJae Lim */
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def cM(((m: Metro, ml: List[Name]))): Boolean = {
    m match {
      case STATION(n0) => { ml.contains(n0) }
      case AREA(n0, m0) => { cM(m0, ml ++ List(n0)) }
      case CONNECT(m0, m1) => { cM(m0, ml) && cM(m1, ml) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { cM(m, Nil()) }
}