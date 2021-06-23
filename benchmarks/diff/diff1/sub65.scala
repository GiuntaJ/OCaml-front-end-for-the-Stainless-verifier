import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub65 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  def diff(((a, str))) = {
    a match {
      case CONST(i) => { CONST(0) }
      case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
      case TIMES(ael) => {
        ael match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(hd, Nil()) => { diff(hd, str) }
          case Cons(hd, tl) => {
            SUM(
              List(TIMES(diff(hd, str) :: tl),
               TIMES(List(hd, diff(TIMES(tl), str)))))
          }
        }
      }
      case SUM(ael) => {
        ael match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(hd, Nil()) => { diff(hd, str) }
          case Cons(hd, tl) => { SUM(List(diff(hd, str), diff(SUM(tl), str))) }
        }
      }
      case POWER(s, i) => {
        TIMES(List(CONST(i), POWER(s, i - 1), diff(VAR(s), str)))
      }
    }
  }
}