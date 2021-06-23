import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub49 {
  sealed case class InvalidArgument() extends Exception {}
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((ae, x))) = {
    ae match {
      case CONST(_) => { CONST(0) }
      case VAR(v) => { if (v == x) CONST(1) else CONST(0) }
      case POWER(v, p) => {
        if (v == x) TIMES(List(CONST(p), POWER(v, p - 1))) else CONST(0)
      }
      case TIMES(lst) => {
        lst match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(head, Nil()) => { diff(head, x) }
          case Cons(head, tail) => {
            SUM(
              List(TIMES(diff(head, x) :: tail),
               TIMES(List(head, diff(TIMES(tail), x)))))
          }
        }
      }
      case SUM(lst) => {
        lst match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(head, Nil()) => { diff(head, x) }
          case Cons(head, tail) => {
            SUM(List(diff(head, x), diff(SUM(tail), x)))
          }
        }
      }
    }
  }
}