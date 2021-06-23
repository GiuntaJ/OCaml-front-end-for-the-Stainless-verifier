import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub1 {
  sealed case class EMPTYLIST() extends Exception {}
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
   def diff(((ae, str))) = {
    ae match {
      case CONST(a) => { CONST(0) }
      case VAR(b) => { if (str == b) CONST(1) else CONST(0) }
      case POWER(pstr, i) => {
        if (str == pstr) TIMES(List(CONST(i), POWER(pstr, i - 1))) else CONST(0)
      }
      case TIMES(alst) => {
        alst match {
          case Nil() => { CONST(0) }
          case Cons(h, Nil()) => { diff(h, str) }
          case Cons(h, t) => {
            SUM(
              List(TIMES(diff(h, str) :: t),
               TIMES(List(h, diff(TIMES(t), str)))))
          }
        }
      }
      case SUM(Nil()) => { CONST(0) }
      case SUM(Cons(h, Nil())) => { CONST(0) }
      case SUM(Cons(h, t)) => { SUM(List(diff(h, str), diff(SUM(t), str))) }
    }
  }
}