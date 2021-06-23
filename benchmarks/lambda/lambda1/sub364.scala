import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub364 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checker(a: Name, b: Name): Boolean = { if (a == b) true else false }
  
  def checkMetroHelper(((a: Metro, l: List[Name]))): Boolean = {
    a match {
      case STATION(x) => { l.exists(checker(x)) }
      case AREA(x, y) => { checkMetroHelper(y, x :: l) }
      case CONNECT(x, y) => { checkMetroHelper(x, l) && checkMetroHelper(y, l) }
    }
  }
  
  def checkMetro(a: Metro): Boolean = { checkMetroHelper(a, Nil()) }
}