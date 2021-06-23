import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub193 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def asdf(met: Metro, l1: List[Name]): Boolean = {
    met match {
      case STATION(a) => { if (l1.contains(a) == true) true else false }
      case CONNECT(m1, m2) => { asdf(m1, l1) && asdf(m2, l1) }
      case AREA(ne, mt) => { asdf(mt, ne :: l1) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    m match {
      case STATION(a) => { false }
      case AREA(name, metro) => { asdf(m, Nil()) }
      case CONNECT(m1, m2) => { asdf(m1, Nil()) && asdf(m2, Nil()) }
    }
  }
  
}
