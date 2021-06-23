import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub189 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def ncor[A](a: A, b: A): Boolean = { a != b }
  
  def slist(l: Metro): List[Name] = {
    l match {
      case STATION(s) => { List(s) }
      case AREA(n, m) => { slist(m).filter(ncor(n)) }
      case CONNECT(m1, m2) => { slist(m1) ++(slist(m2)) }
    }
  }
  def checkMetro(metro: Metro): Boolean = {
    slist(metro) match {
      case Nil() => { true }
      case _ => { false }
    }
  }
}