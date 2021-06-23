import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub486 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def same(y, x) = { x == y }
  
  def checkMetroName(((x, y))) = {
    y match {
      case STATION(a) => { x.exists(same(a)) }
      case AREA(a, b) => { checkMetroName(x ++(List(a)), b) }
      case CONNECT(a, b) => { checkMetroName(x, a) && checkMetroName(x, b) }
    }
  }
  
  def checkMetro(x: Metro): Boolean = {
    x match {
      case STATION(a) => { false }
      case CONNECT(a, b) => { checkMetro(a) && checkMetro(b) }
      case AREA(a, b) => { checkMetroName(List(a), b) }
    }
  }
}
