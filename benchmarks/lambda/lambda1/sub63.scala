import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub63 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetro(metro) = {
    metro match {
      case AREA(a, STATION(b)) => { a == b }
      case AREA(a, CONNECT(b, c)) => {
        checkMetro(AREA(a, b)) && checkMetro(AREA(a, c))
      }
      case AREA(a, AREA(b, c)) => { checkMetro(AREA(a, c)) }
      case CONNECT(_, _) => { false }
      case STATION(_) => { false }
    }
  }
}