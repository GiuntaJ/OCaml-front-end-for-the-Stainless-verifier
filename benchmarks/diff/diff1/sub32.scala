import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub32 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((x, y))) = {
    x match {
      case CONST(c) => { CONST(0) }
      case VAR(a) => { if (a == y) CONST(1) else CONST(0) }
      case POWER(a, b) => {
        
          if (
            a ne y
          ) {
            CONST(0) 
          } else if (
            b == 1
          ) {
            VAR(a) 
          } else if (
            b == 2
          ) {
            TIMES(List(CONST(2), VAR(a))) 
          } else {
            TIMES(List(CONST(b), POWER(a, b - 1)))
          }
      }
      case SUM(l) => {
        l match {
          case Nil() => { CONST(0) }
          case Cons(h, t) => { SUM(List(diff(h, y), diff(SUM(t), y))) }
        }
      }
      case TIMES(l) => {
        l match {
          case Nil() => { CONST(0) }
          case Cons(h, Nil()) => { diff(h, y) }
          case Cons(CONST(a), t) => { TIMES(List(CONST(a), diff(TIMES(t), y))) }
          case Cons(h, t) => {
            SUM(
              List(TIMES(List(diff(h, y), TIMES(t))),
               TIMES(List(h, diff(TIMES(t), y)))))
          }
        }
      }
    }
  }
}