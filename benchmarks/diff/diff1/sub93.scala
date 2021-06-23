import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub93 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff: (Ae, String) => Ae = {
    case (ex, v) =>
      {
        (ex, v) match {
          case (CONST(a), _) => { CONST(0) }
          case (VAR(s), v) => { if (s != v) CONST(0) else CONST(1) }
          case (POWER(s, e), v) => {
            
              if (
                s != v
              ) {
                CONST(0) 
              } else if (
                e == 0
              ) {
                CONST(0) 
              } else {
                TIMES(List(CONST(e), POWER(s, e - 1)))
              }
          }
          case (TIMES(a), v) => {
            a match {
              case Nil() => { assert(false, "InvalidArgument") }
              case Cons(single, Nil()) => { diff(single, v) }
              case _ => {
                SUM(
                  List(TIMES(diff(a.head, v) :: a.tail),
                   TIMES(List(a.head, diff(TIMES(a.tail), v)))))
              }
            }
          }
          case (SUM(a), v) => {
            a match {
              case Nil() => { assert(false, "InvalidArgument") }
              case Cons(single, Nil()) => { diff(single, v) }
              case _ => { SUM(List(diff(a.head, v), diff(SUM(a.tail), v))) }
            }
          }
        }
    }
  }
}
