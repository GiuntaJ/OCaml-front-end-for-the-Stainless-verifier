import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub242 {
  type Name = String
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def hasName(((n, l))) = {
    l match {
      case Cons(a, b) => { a == n || hasName(n, b) }
      case Nil() => { false }
    }
  }
  
  def isMetro(((m, l))) = {
    m match {
      case STATION(n) => { hasName(n, l) }
      case AREA(n, nm) => { isMetro(nm, n :: l) }
      case CONNECT(nm1, nm2) => { isMetro(nm1, l) && isMetro(nm2, l) }
    }
  }
  
  def checkMetro(m) = { isMetro(m, Nil()) }
}