import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub43 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff: (Ae, String) => Ae = {
    case (expr, str) =>
      {
        expr match {
          case CONST(i) => { CONST(0) }
          case VAR(s) => { if (str ne s) CONST(0) else CONST(1) }
          case POWER(s, i) => {
            if (str ne s) CONST(0) else TIMES(List(CONST(i), POWER(s, i - 1)))
          }
          case TIMES(l) => {
            l match {
              case Nil() => { CONST(0) }
              case Cons(h, t) => {
                t match {
                  case Nil() => { TIMES(List(diff(h, str))) }
                  case _ => {
                    SUM(
                      List(TIMES(diff(h, str) :: t),
                       TIMES(List(h, diff(TIMES(t), str)))))
                  }
                }
              }
            }
          }
          case SUM(l) => {
            l match {
              case Nil() => { CONST(0) }
              case Cons(h, t) => {
                t match {
                  case Nil() => { diff(h, str) }
                  case _ => { SUM(List(diff(h, str), diff(SUM(t), str))) }
                }
              }
            }
          }
        }
    }
  }
  
}
