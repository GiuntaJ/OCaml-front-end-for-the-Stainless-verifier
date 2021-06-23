import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub427 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def cMIter: (Metro, List[Name]) => Boolean = {
    case (m, l) =>
      {
        m match {
          case STATION(n) => { l.contains(n) }
          case AREA(n, nm) => { cMIter(nm, n :: l) }
          case CONNECT(a, b) => { cMIter(a, l) && cMIter(b, l) }
        }
    }
  }
  
  val checkMetro: Metro => Boolean = ( (m) => { cMIter(m, Nil()) } )
}