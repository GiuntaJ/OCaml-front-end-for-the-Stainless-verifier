import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub431 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkMetroImpl[A](m: Metro, l: List[A]): Boolean = {
    m match {
      case STATION(n) => { l.contains(n) }
      case AREA(n, mm) => { checkMetroImpl(mm, n :: l) }
      case CONNECT(m1, m2) => { checkMetroImpl(m1, l) && checkMetroImpl(m2, l) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkMetroImpl(m, Nil()) }
}