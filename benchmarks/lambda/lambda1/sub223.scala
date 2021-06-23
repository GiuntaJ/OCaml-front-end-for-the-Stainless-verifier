import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub223 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  sealed abstract class Passlist {}
  case class ONE(param0: String) extends Passlist {}
  case class LIST(param0: Passlist,  param1: String) extends Passlist {}
  	
  def stationCheck: (Passlist, String) => Boolean = {
    case (l, s) =>
      {
        l match {
          case ONE(a) => { a == s }
          case LIST(a, b) => { b == s || stationCheck(a, s) }
        }
    }
  }
  	
  def checkName: (Passlist, Metro) => Boolean = {
    case (l, m) =>
      {
        m match {
          case STATION(a) => { stationCheck(l, a) }
          case AREA(a, b) => { checkName(LIST(l, a), b) }
          case CONNECT(a, b) => { checkName(l, a) && checkName(l, b) }
        }
    }
  }
  	
  def checkMetro: Metro => Boolean = (
    (m) =>
      {
        m match {
          case STATION(a) => { false }
          case AREA(a, b) => { checkName(ONE(a), b) }
          case CONNECT(a, b) => { checkMetro(a) && checkMetro(b) }
        }
    }
  )
}