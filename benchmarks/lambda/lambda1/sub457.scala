import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub457 {
  type Name = String
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  def check(metro: Metro, env: List[Name]): Boolean = {
    metro match {
      case STATION(s) => {
        env match {
          case Cons(h, t) => { if (h == s) true else check(metro, t) }
          case Nil() => { false }
        }
      }
      case AREA(n, m) => { check(m, n :: env) }
      case CONNECT(m1, m2) => { check(m1, env) && check(m2, env) }
    }
  }
  
  def checkMetro(metro: Metro): Boolean = {
    val _2 = {
      val areal = Nil()
      metro match {
        case STATION(s) => { false }
        case _ => { check(metro, areal) }
      }
    }
  }
}
