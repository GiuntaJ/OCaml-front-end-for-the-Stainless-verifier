import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub99 {
  /* Ex8 */
  sealed abstract class Partial {}
  case object FIRSTHALF extends Partial {}
  case object SECONDHALF extends Partial {}
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkMetro(metro: Metro): Boolean = {
    metro match {
      case STATION(_) => { false }
      case AREA(a, STATION(s)) => { if (a == s) true else false }
      case AREA(a, AREA(a2, CONNECT(m1, m2))) => {
        checkMetro(AREA(a2, CONNECT(m1, m2))) ||
        checkMetro(AREA(a, m1)) && checkMetro(AREA(a2, m2)) ||
        checkMetro(AREA(a, m2)) && checkMetro(AREA(a2, m1)) ||
        checkMetro(AREA(a, m1)) && checkMetro(AREA(a, m2))
      }
      case AREA(a, AREA(a2, m)) => {
        checkMetro(m) || checkMetro(AREA(a2, m)) || checkMetro(AREA(a, m))
      }
      case AREA(a, CONNECT(m1, m2)) => {
        checkMetro(AREA(a, m1)) && checkMetro(AREA(a, m2))
      }
      case CONNECT(STATION(_), _) => { false }
      case CONNECT(_, STATION(_)) => { false }
      case CONNECT(AREA(a1, m1), AREA(a2, m2)) => {
        checkMetro(AREA(a1, m1)) && checkMetro(AREA(a2, m2))
      }
      case CONNECT(CONNECT(m1, m2), CONNECT(m3, m4)) => {
        checkMetro(m1) && checkMetro(m2) && checkMetro(m3) && checkMetro(m4)
      }
      case CONNECT(CONNECT(m1, m2), AREA(a, m)) => {
        checkMetro(m1) && checkMetro(m2) && checkMetro(AREA(a, m))
      }
      case CONNECT(AREA(a, m), CONNECT(m1, m2)) => {
        checkMetro(AREA(a, m)) && checkMetro(m1) && checkMetro(m2)
      }
    }
  }
}