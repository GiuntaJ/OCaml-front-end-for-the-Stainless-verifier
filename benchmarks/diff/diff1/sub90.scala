import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub90 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  sealed case class InvalidArgument() extends Exception {}
  def diff(((ae, dx))) = {
    ae match {
      case CONST(a) => { CONST(0) }
      case VAR(a) => { if (a == dx) CONST(1) else CONST(0) }
      case POWER(a, b) => {
        
          if (
            a == dx
          ) {
            if (b == 0) CONST(0) else TIMES(List(CONST(b), POWER(a, b - 1))) 
          } else {
            CONST(0)
          }
      }
      case TIMES(lst) => {
        lst match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(one, Nil()) => { diff(one, dx) }
          case _ => {
            SUM(
              List(TIMES(diff(lst.head, dx) :: lst.tail),
               TIMES(List(lst.head, diff(TIMES(lst.tail), dx)))))
          }
        }
      }
      case SUM(lst) => {
        
          if (
            Nil() == lst
          ) {
            assert(false, "InvalidArgument") 
          } else {
            SUM(lst.map(( (a) => { diff(a, dx) } )))
          }
      }
    }
  }
}