import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub56 {
  sealed case class InvalidArgument() extends Exception {}
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((ae, var0))) = {
    ae match {
      case CONST(c) => { CONST(0) }
      case VAR(v) => { if (v == var0) CONST(1) else CONST(0) }
      case POWER(v, n) => {
        
          if (
            not(n == 0) && v == var0
          ) {
            TIMES(List(CONST(n), POWER(v, n - 1))) 
          } else {
            CONST(0)
          }
      }
      case SUM(li) => {
        li match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(hd, Nil()) => { diff(hd, var0) }
          case Cons(hd, tl) => { SUM(List(diff(hd, var0), diff(SUM(tl), var0)))
          }
        }
      }
      case TIMES(li) => {
        li match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(hd, Nil()) => { diff(hd, var0) }
          case Cons(hd, tl) => {
            SUM(
              List(TIMES(List(diff(hd, var0), TIMES(tl))),
               TIMES(List(hd, diff(TIMES(tl), var0)))))
          }
        }
      }
    }
  }
}
