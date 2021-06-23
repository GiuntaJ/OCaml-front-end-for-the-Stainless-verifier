import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub57 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def compareList(((name, l))) = { l.contains(name) }
  
  def checkMetroTail(((metro, l))) = {
    metro match {
      case STATION(x) => { compareList(x, l) }
      case AREA(x, y) => { checkMetroTail(y, x :: l) }
      case CONNECT(x, y) => { checkMetroTail(x, l) && checkMetroTail(y, l) }
    }
  }
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      val l = Nil()
      metro match {
        case STATION(x) => { compareList(x, l) }
        case AREA(x, y) => { checkMetroTail(y, x :: l) }
        case CONNECT(x, y) => { checkMetroTail(x, l) && checkMetroTail(y, l) }
      }
    }
  }
}