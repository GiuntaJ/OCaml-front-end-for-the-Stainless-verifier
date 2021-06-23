import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub307 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def contains(((n: Name, l: List[Name]))): Boolean = {
    l match {
      case Nil() => { false }
      case Cons(h, t) => { if (n == h) true else contains(n, t) }
    }
  }
  
  def checkMetrosub(((m: Metro, l: List[Name]))): Boolean = {
    m match {
      case STATION(n) => { contains(n, l) }
      case AREA(n, m) => { checkMetrosub(m, List(n) ++ l) }
      case CONNECT(m1, m2) => { checkMetrosub(m1, l) && checkMetrosub(m2, l) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = {
    m match {
      case STATION(n) => { false }
      case AREA(n, m) => { checkMetrosub(m, List(n)) }
      case CONNECT(m1, m2) => {
        checkMetrosub(m1, Nil()) && checkMetrosub(m2, Nil())
      }
    }
  }
}