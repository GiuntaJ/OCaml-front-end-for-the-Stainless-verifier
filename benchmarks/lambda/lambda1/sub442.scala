import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub442 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro2: (Metro, List[String]) => Boolean = {
    case (m, l) =>
      {
        m match {
          case STATION(a) => { l.contains(a) }
          case AREA(a, m2) => { checkMetro2(m2, a :: l) }
          case CONNECT(m1, m2) => { checkMetro2(m1, l) && checkMetro2(m2, l) }
        }
    }
  }
  
  val checkMetro: Metro => Boolean = ( (m) => { checkMetro2(m, Nil()) } )
}