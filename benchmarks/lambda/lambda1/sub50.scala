import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub50 {
  sealed case class Error(param0: String) extends Exception {}
  
  def inclu[A](l: List[A], a: A): Boolean = {
    l match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == a) true else inclu(tl, a) }
    }
  }
  
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checker(l: List[Name], m: Metro): Boolean = {
    m match {
      case STATION(n) => { inclu(l, n) }
      case AREA(n, m1) => { checker(n :: l, m1) }
      case CONNECT(m1, m2) => {
        (checker(l, m1), checker(l, m2)) match {
          case (true, true) => { true }
          case (_, _) => { false }
        }
      }
    }
  }
  def checkMetro(m: Metro): Boolean = { checker(Nil(), m) }
}