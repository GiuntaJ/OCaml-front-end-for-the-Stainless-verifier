import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub272 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String 
  
  def contain(((l, str))) = {
    l match {
      case Nil() => { false }
      case Cons(h, t) => { h == str || contain(t, str) }
    }
  }
  
  def checkMetro2(((s, m))) = {
    m match {
      case STATION(x) => { contain(s, x) }
      case CONNECT(m1, m2) => { checkMetro2(s, m1) && checkMetro2(s, m2) }
      case AREA(a, b) => { checkMetro2(a :: s, b) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkMetro2(Nil(), m) }	
}