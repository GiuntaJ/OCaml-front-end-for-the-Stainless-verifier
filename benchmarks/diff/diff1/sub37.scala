import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub37 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  val diff: (Ae, String) => Ae = val _0 = {
    def indiff(((x, y))) = {
      x match {
        case CONST(_) => { CONST(0) }
        case VAR(a) => { if (a == y) CONST(1) else CONST(0) }
        case POWER(a, b) => {
          if (a == y) TIMES(List(CONST(b), POWER(a, b - 1))) else CONST(0)
        }
        case SUM(x) => {
          val _6 = {
            def sm(x) = {
              x match {
                case Cons(h, t) => { indiff(h, y) :: sm(t) }
                case _ => { Nil() }
              }
            }
            SUM(sm(x))
          }
        }
        case TIMES(x) => {
          val _3 = {
            def tms(w, x) = {
              x match {
                case Cons(h, t) => {
                  TIMES(w ++ indiff(h, y) :: t) :: tms(w ++ List(h), t)
                }
                case _ => { Nil() }
              }
            }
            SUM(tms(Nil(), x))
          }
        }
      }
    }
    {
      case (x, y) => { indiff(x, y) }
    }
  }
}