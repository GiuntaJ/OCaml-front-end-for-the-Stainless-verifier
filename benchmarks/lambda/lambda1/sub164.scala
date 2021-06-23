import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub164 {
  type Name = String
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def isStationIn(((areas, n))) = {
    areas match {
      case Nil() => { false }
      case Cons(hd, tl) => { hd == n || isStationIn(tl, n) }
    }
  }
  
  def checkMetro_real(((areas, metro))) = {
    metro match {
      case AREA(a, m) => { checkMetro_real(a :: areas, m) }
      case STATION(n) => { isStationIn(areas, n) }
      case CONNECT(a, b) => {
        checkMetro_real(areas, a) && checkMetro_real(areas, b)
      }
    }
  }
  
  def checkMetro(metro: Metro): Boolean = { checkMetro_real(Nil(), metro) }
}