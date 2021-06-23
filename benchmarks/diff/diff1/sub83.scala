import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub83 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((eq, var0))) = {
    eq match {
      case CONST(n) => { CONST(0) }
      case VAR(x) => { if (x == var0) CONST(1) else CONST(0) }
      case POWER(x, n) => {
        if (x == var0) TIMES(List(CONST(n), POWER(x, n - 1))) else CONST(0)
      }
      case TIMES(lst) => {
        lst match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(head, Nil()) => { diff(head, var0) }
          case Cons(head, tail) => {
            SUM(
              List(TIMES(diff(head, var0) :: tail),
               TIMES(List(head, diff(TIMES(tail), var0)))))
          }
        }
      }
      case SUM(lst) => {
        lst match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(head, Nil()) => { diff(head, var0) }
          case Cons(head, tail) => {
            SUM(List(diff(head, var0), diff(SUM(tail), var0)))
          }
        }
      }
    }
  }
}