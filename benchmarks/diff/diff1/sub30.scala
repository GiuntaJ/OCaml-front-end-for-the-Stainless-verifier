import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub30 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((exp, d))) = {
    exp match {
      case CONST(i) => { CONST(0) }
      case VAR(str) => { if (str == d) CONST(1) else CONST(0) }
      case POWER(str, i) => {
        
          if (
            i eq 0
          ) {
            CONST(0) 
          } else if (
            i eq 1 && str == d
          ) {
            CONST(1) 
          } else if (
            str == d
          ) {
            TIMES(List(CONST(i), POWER(str, i - 1))) 
          } else {
            CONST(0)
          }
      }
      case SUM(sList) => {
        sList match {
          case Nil() => { CONST(0) }
          case Cons(hd, Nil()) => { diff(hd, d) }
          case Cons(hd, tl) => { SUM(List(diff(hd, d), diff(SUM(tl), d))) }
        }
      }
      case TIMES(tList) => {
        tList match {
          case Nil() => { CONST(0) }
          case Cons(hd, Nil()) => { diff(hd, d) }
          case Cons(hd, tl) => {
            SUM(
              List(TIMES(diff(hd, d) :: tl),
               TIMES(List(hd, diff(TIMES(tl), d)))))
          }
        }
      }
    }
  }
}