import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub69 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((expr, variable))) = {
    expr match {
      case CONST(i) => { CONST(0) }
      case VAR(str) => { if (variable == str) CONST(1) else CONST(0) }
      case POWER(str, i) => {
        
          if (
            variable == str
          ) {
            TIMES(List(CONST(i), POWER(str, i - 1))) 
          } else {
            CONST(0)
          }
      }
      case TIMES(l) => {
        l match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(hd, Nil()) => { diff(hd, variable) }
          case Cons(hd, tl) => {
            SUM(
              List(TIMES(diff(hd, variable) :: tl),
               TIMES(List(hd, diff(TIMES(tl), variable)))))
          }
        }
      }
      case SUM(l) => {
        l match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(hd, Nil()) => { diff(hd, variable) }
          case Cons(hd, tl) => {
            SUM(List(diff(hd, variable), diff(SUM(tl), variable)))
          }
        }
      }
    }
  }
}
