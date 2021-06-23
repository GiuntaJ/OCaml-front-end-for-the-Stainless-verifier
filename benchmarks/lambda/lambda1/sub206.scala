import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub206 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  def check(metro: Metro, namelist: List[Name]): Boolean = {
    metro match {
      case STATION(name) => { namelist.exists(( (x) => { x == name } )) }
      case AREA(a, b) => { check(b, a :: namelist) }
      case CONNECT(a, b) => { check(a, namelist) && check(b, namelist) }
    }
  }
  def checkMetro(metro: Metro): Boolean = { check(metro, Nil()) }
}