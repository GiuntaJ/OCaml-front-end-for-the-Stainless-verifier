import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub167 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def check(namelist: List[Name], metro: Metro): Boolean = {
    metro match {
      case STATION(n) => { if (namelist.contains(n)) true else false }
      case AREA(n, m) => { check(List(n) ++(namelist), m) }
      case CONNECT(m1, m2) => { check(namelist, m1) && check(namelist, m2) }
    }
  }
  
  def checkMetro(metro: Metro): Boolean = {
    metro match {
      case STATION(n) => { false }
      case AREA(n, m) => { check(List(n), m) }
      case CONNECT(m1, m2) => { checkMetro(m1) && checkMetro(m2) }
    }
  }
}