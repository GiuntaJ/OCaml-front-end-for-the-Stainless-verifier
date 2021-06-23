import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub284 {
  /* Department: Electrical and Computer Engineering */
  /* Student ID: 2010-11834 */
  /* Name: Kwonjoon Lee */
  /* Exercise #3 */
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetroAux(((m, l))): Boolean = {
    m match {
      case AREA(x, y) => { checkMetroAux(y, x :: l) }
      case CONNECT(x, y) => { checkMetroAux(x, l) && checkMetroAux(y, l) }
      case STATION(x) => { l.contains(x) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkMetroAux(m, Nil()) }
}