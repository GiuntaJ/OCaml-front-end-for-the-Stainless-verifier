import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub448 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def isInclude: (Name, Metro) => Boolean = {
    case (n, m) =>
      {
        m match {
          case STATION(a) => { n eq a }
          case AREA(a, b) => { isInclude(a, b) && isInclude(n, b) }
          case CONNECT(a, b) => { isInclude(n, a) || isInclude(n, b) }
        }
    }
  }
  
  val checkMetro: Metro => Boolean = (
    (x) =>
      {
        x match {
          case AREA(a, STATION(b)) => { a eq b }
          case AREA(a, b) => { isInclude(a, b) }
          case _ => { false }
        }
    }
  )
}