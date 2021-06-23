import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub175 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkPos[A](name: A, area: List[A]): Boolean = {
    area match {
      case Cons(hd, tl) => { if (hd == name) true else checkPos(name, tl) }
      case Nil() => { false }
    }
  }
  
  def subfunc(m: Metro, area: List[Name]): Boolean = {
    m match {
      case AREA(n, msub) => { subfunc(msub, area ++ List(n)) }
      case STATION(n) => { checkPos(n, area) }
      case CONNECT(n1, n2) => { subfunc(n1, area) && subfunc(n2, area) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { subfunc(m, Nil()) }
}
