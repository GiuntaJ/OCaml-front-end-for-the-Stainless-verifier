import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub465 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  
  def matchName(n: Name, l: List[Name]): Boolean = {
    l match {
      case Nil() => { false }
      case Cons(h, t) => { if (n == h) true else matchName(n, t) }
    }
  }
  
  def checkMetroRec(m: Metro, areas: List[Name]): Boolean = {
    m match {
      case STATION(n) => { matchName(n, areas) }
      case CONNECT(m1, m2) => {
        checkMetroRec(m1, areas) && checkMetroRec(m2, areas)
      }
      case AREA(n, m) => { checkMetroRec(m, n :: areas) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkMetroRec(m, Nil()) }
}