import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub160 {
  type Name = String
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def apl(((b, m))) = {
    (b, m) match {
      case (b, STATION(a)) => { if (b == a) AREA(b, STATION(a)) else STATION(a)
      }
      case (b, AREA(a, STATION(c))) => {
        if (a ne c && b == c) AREA(b, STATION(c)) else AREA(a, STATION(c))
      }
      case (b, AREA(a, AREA(c, d))) => { AREA(a, AREA(c, apl(b, d))) }
      case (b, AREA(a, CONNECT(c, d))) => { AREA(a, apl(b, CONNECT(c, d))) }
      case (b, CONNECT(a, c)) => { CONNECT(apl(b, a), apl(b, c)) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    m match {
      case AREA(a, STATION(b)) => { if (a == b) true else false }
      case AREA(a, CONNECT(b, c)) => {
        checkMetro(AREA(a, b)) && checkMetro(AREA(a, c))
      }
      case AREA(a, AREA(b, STATION(c))) => {
        checkMetro(AREA(a, STATION(c))) || checkMetro(AREA(b, STATION(c)))
      }
      case AREA(a, AREA(b, c)) => {
        checkMetro(apl(b, AREA(a, c))) || checkMetro(apl(a, AREA(b, c)))
      }
      case CONNECT(a, b) => { checkMetro(a) && checkMetro(b) }
      case _ => { false }
    }
  }
}