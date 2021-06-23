import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub151 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def inlist[A](a: A, l: List[A]): Boolean = {
    l match {
      case Nil() => { false }
      case Cons(h, t) => { if (a == h) true else inlist(a, t) }
    }
  }
  
  def checkm(l: List[Name], m: Metro): Boolean = {
    m match {
      case STATION(a) => { inlist(a, l) }
      case AREA(a, b) => { checkm(a :: l, b) }
      case CONNECT(a, b) => { checkm(l, a) && checkm(l, b) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkm(Nil(), m) }
}