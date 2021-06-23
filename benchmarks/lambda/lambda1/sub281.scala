import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub281 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checkArea(((e, m))) = {
    m match {
      case STATION(s) => {
        e match {
          case Nil() => { false }
          case Cons(h, Nil()) => { if (s == h) true else false }
          case Cons(h, t) => { if (s == h) true else checkArea(t, STATION(s)) }
        }
      }
      case CONNECT(m1, m2) => {
        if (checkArea(e, m1) && checkArea(e, m2)) true else false
      }
      case AREA(a, m) => { checkArea(a :: e, m) }
    }
  }
  
  def checkMetro(m: Metro): Boolean = { checkArea(Nil(), m) }
}
