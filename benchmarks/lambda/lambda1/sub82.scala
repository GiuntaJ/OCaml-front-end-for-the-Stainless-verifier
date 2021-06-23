import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub82 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(a: Metro): Boolean = {
    a match {
      case STATION(_) => { assert(false, "Invalid_argument with checkMetro") }
      case CONNECT(a, b) => { assert(false, "Invalid_argument with checkMetro")
      }
      case AREA(a, STATION(b)) => { if (a == b) true else false }
      case AREA(a, CONNECT(STATION(b), STATION(c))) => {
        if (a == b || a == c) true else false
      }
      case AREA(a, CONNECT(AREA(b, STATION(c)), STATION(d))) => {
        if (a == c || a == d) true else false
      }
      case AREA(a, CONNECT(STATION(b), AREA(c, STATION(d)))) => {
        if (a == b || a == d) true else false
      }
      case AREA(a, b) => { checkMetro(b) }
    }
  }
}