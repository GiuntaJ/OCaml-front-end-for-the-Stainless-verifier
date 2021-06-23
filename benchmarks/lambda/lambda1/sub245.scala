import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub245 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkInner: (Metro, List[String]) => Boolean = {
    case (m, sl) =>
      {
        m match {
          case STATION(n) => { sl.contains(n) }
          case AREA(n, m) => { checkInner(m, n :: sl) }
          case CONNECT(m1, m2) => { checkInner(m1, sl) && checkInner(m2, sl) }
        }
    }
  }
  
  val checkMetro: Metro => Boolean = ( (m) => { checkInner(m, Nil()) } )
}