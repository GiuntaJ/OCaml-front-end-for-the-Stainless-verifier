import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub344 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def checkarea(a: List[String], s: String): Boolean = {
    a match {
      case Nil() => { false }
      case Cons(h, t) => { if (s == h) true else checkarea(t, s) }
    }
  }
  
  def areacount(m: Metro, a: List[String]): Boolean = {
    m match {
      case STATION(s) => { checkarea(a, s) }
      case AREA(s, m) => { areacount(m, s :: a) }
      case CONNECT(m1, m2) => { areacount(m1, a) && areacount(m2, a) }
    }
  }
  
  	
  def checkMetro(m: Metro): Boolean = { areacount(m, Nil()) }
}