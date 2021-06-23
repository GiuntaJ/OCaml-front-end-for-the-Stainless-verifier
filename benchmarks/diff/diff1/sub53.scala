import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub53 {
  sealed case class InvalidArgument() extends Exception {}
  sealed case class L2AL_null() extends Exception {}
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  def diff(((ae, str))) = {
    val _2 = {
      def list2aelist(((aelist, st))) = {
        aelist match {
          case Cons(h, Nil()) => { List(diff(h, st)) }
          case Cons(h, t) => { List(diff(h, st)) ++ list2aelist(t, st) }
          case Nil() => { assert(false, "L2AL_null") }
        }
      }
      ae match {
        case CONST(c) => { CONST(0) }
        case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
        case POWER(s, n) => {
          
            if (
              n eq 0
            ) {
              CONST(1) 
            } else if (
              n eq 1 && s == str
            ) {
              CONST(2) 
            } else if (
              n eq 1 && s ne str
            ) {
              CONST(3) 
            } else if (
              s == str
            ) {
              TIMES(List(CONST(n), POWER(s, n - 1))) 
            } else {
              CONST(0)
            }
        }
        case SUM(aelist) => {
          aelist match {
            case Nil() => { assert(false, "InvalidArgument") }
            case Cons(h, Nil()) => { diff(h, str) }
            case Cons(h, t) => { SUM(List(diff(h, str)) ++ list2aelist(t, str))
            }
          }
        }
        case TIMES(aelist) => {
          aelist match {
            case Nil() => { assert(false, "InvalidArgument") }
            case Cons(h, Nil()) => { diff(h, str) }
            case Cons(h, t) => {
              SUM(
                List(TIMES(List(diff(h, str)) ++ t),
                 TIMES(List(h) ++ List(diff(TIMES(t), str)))))
            }
          }
        }
      }
    }
  }
}