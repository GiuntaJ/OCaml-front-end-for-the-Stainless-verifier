import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub201 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def licheck[A](s1: A, lst1: List[A]): Boolean = {
    lst1 match {
      case Cons(head, tail) => { s1 == head || licheck(s1, tail) }
      case Nil() => { false }
    }
  }
  
  def sicheck(m: Metro, lst: List[Name]): Boolean = {
    m match {
      case AREA(s, m1) => { sicheck(m1, s :: lst) }
      case CONNECT(m1, m2) => { sicheck(m1, lst) && sicheck(m2, lst) }
      case STATION(s) => { licheck(s, lst) }
    }
  }
  
  def checkMetro(mtr: Metro): Boolean = { sicheck(mtr, Nil()) }
}