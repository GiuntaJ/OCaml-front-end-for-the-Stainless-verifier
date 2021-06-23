import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub47 {
  sealed case class InvalidArgument() extends Exception {}
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  val diff: (Ae, String) => Ae = {
    case (e, str) =>
      {
        val _2 = {
          def diff_sub(((e, str))) = {
            e match {
              case CONST(n) => { CONST(0) }
              case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
              case POWER(str, n) => { TIMES(List(CONST(n), POWER(str, n - 1))) }
              case TIMES(lst) => {
                lst match {
                  case Nil() => { assert(false, "InvalidArgument") }
                  case Cons(a, Nil()) => { diff_sub(a, str) }
                  case Cons(a, t) => {
                    SUM(
                      List(TIMES(List(diff_sub(a, str), TIMES(t))),
                       TIMES(List(a, diff_sub(TIMES(t), str)))))
                  }
                }
              }
              case SUM(lst) => {
                lst match {
                  case Nil() => { assert(false, "InvalidArgument") }
                  case Cons(a, Nil()) => { diff_sub(a, str) }
                  case Cons(a, t) => {
                    SUM(List(diff_sub(a, str), diff_sub(SUM(t), str)))
                  }
                }
              }
            }
          }
          diff_sub(e, str)
        }
    }
  }
}