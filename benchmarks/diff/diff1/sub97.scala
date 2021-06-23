import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub97 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((ae, str))) = {
    ae match {
      case CONST(c) => { CONST(0) }
      case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
      case POWER(s, c) => {
        if (s == str) TIMES(List(POWER(s, c - 1), CONST(c))) else CONST(0)
      }
      case TIMES(li) => {
        li match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(head, Nil()) => { diff(head, str) }
          case Cons(head, tail) => {
            SUM(
              List(TIMES(List(diff(head, str), TIMES(tail))),
               TIMES(List(head, diff(TIMES(tail), str)))))
          }
        }
      }
      case SUM(l) => {
        l match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(head, Nil()) => { diff(head, str) }
          case Cons(head, tail) => {
            SUM(List(diff(head, str), diff(SUM(tail), str)))
          }
        }
      }
    }
  }
}