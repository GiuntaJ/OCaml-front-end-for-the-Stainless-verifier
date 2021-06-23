import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub150 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def isthere[A](s: A, lst: List[A]): Boolean = {
    lst match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == s) true else isthere(s, tl) }
    }
  }
  
  def check(mtr: Metro, lst: List[Name]): Boolean = {
    mtr match {
      case STATION(x) => { isthere(x, lst) }
      case AREA(str, m) => { check(m, str :: lst) }
      case CONNECT(m1, m2) => { check(m1, lst) && check(m2, lst) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { check(m, Nil()) }
  
}
