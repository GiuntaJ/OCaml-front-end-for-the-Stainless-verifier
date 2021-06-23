import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub60 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((a, x))) = {
    a match {
      case CONST(a) => { CONST(0) }
      case VAR(a) => { if (a == x) CONST(1) else CONST(0) }
      case POWER(a, b) => {
        if (a == x) TIMES(List(CONST(b), POWER(a, b - 1))) else CONST(0)
      }
      case TIMES(a) => {
        a match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(hd, tl) => {
            
              if (
                tl == Nil()
              ) {
                diff(hd, x) 
              } else {
                SUM(
                  List(TIMES(diff(hd, x) :: tl),
                   TIMES(List(hd, diff(TIMES(tl), x)))))
              }
          }
        }
      }
      case SUM(a) => {
        a match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(hd, tl) => {
            
              if (
                tl == Nil()
              ) {
                diff(hd, x) 
              } else {
                SUM(List(diff(hd, x), diff(SUM(tl), x)))
              }
          }
        }
      }
    }
  }
}