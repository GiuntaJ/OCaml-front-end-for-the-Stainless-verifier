import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_lambda_lambda1_sub126 {
  sealed abstract class Metro {}
  case class STATION(param0: Name) extends Metro {}
  case class AREA(param0: Name,  param1: Metro) extends Metro {}
  case class CONNECT(param0: Metro,  param1: Metro) extends Metro {}
  
  type Name = String
  
  def checklist(((arlist, stlist))) = {
    val _2 = {
      def checkst(((arlst, st))) = {
        (arlst, st) match {
          case (Nil(), _) => { false }
          case (Cons(a, b), c) => { if (a == c) true else checkst(b, c) }
        }
      }
      (arlist, stlist) match {
        case (_, Nil()) => { true }
        case (a, Cons(b, c)) => { if (checkst(a, b)) checklist(a, c) else false
        }
      }
    }
  }
  
  def checkMetro(met: Metro): Boolean = {
    val _5 = {
      def makestlist(me: Metro) = {
        me match {
          case STATION(a) => { List(a) }
          case AREA(a, b) => { makestlist(b) }
          case CONNECT(a, b) => { makestlist(a) ++ makestlist(b) }
        }
      }
      val _6 = {
        def makearlist(me: Metro) = {
          me match {
            case AREA(a, b) => { a :: makearlist(b) }
            case CONNECT(a, b) => {
              (a, b) match {
                case (STATION(c), STATION(d)) => { Nil() }
                case (STATION(c), AREA(d, e)) => { d :: makearlist(e) }
                case (STATION(c), CONNECT(_)) => { makearlist(b) }
                case (AREA(c, d), STATION(e)) => { c :: makearlist(d) }
                case (AREA(c, d), AREA(e, f)) => {
                  c :: e :: makearlist(d) ++ makearlist(f)
                }
                case (AREA(c, d), CONNECT(_)) => {
                  c :: makearlist(d) ++ makearlist(b)
                }
                case (CONNECT(_), STATION(c)) => { makearlist(a) }
                case (CONNECT(_), AREA(c, d)) => {
                  makearlist(a) ++ c :: makearlist(d)
                }
                case (CONNECT(_), CONNECT(_)) => {
                  makearlist(a) ++ makearlist(b)
                }
              }
            }
            case _ => { Nil() }
          }
        }
        met match {
          case AREA(a, b) => { checklist(a :: makearlist(b), makestlist(b)) }
          case CONNECT(a, b) => { checkMetro(a) && checkMetro(b) }
          case _ => { false }
        }
      }
    }
  }
}