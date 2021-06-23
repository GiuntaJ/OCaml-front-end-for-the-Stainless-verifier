import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub197 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def dec(met: Metro, lst: List[Name]): Boolean = {
    met match {
      case STATION(a) => { if (lst.contains(a) == true) true else false }
      case CONNECT(m1, m2) => { dec(m1, lst) && dec(m1, lst) }
      case AREA(n1, m1) => { dec(m1, n1 :: lst) }
    }
  }
    
    def checkMetro(m) = {
    m match {
      case STATION(a) => { false }
      case AREA(n1, m1) => { dec(m, List(n1)) }
      case CONNECT(m1, m2) => { dec(m1, Nil()) && dec(m2, Nil()) }
    }
  }
}