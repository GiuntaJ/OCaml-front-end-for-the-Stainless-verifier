import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub16 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((e, v))) = {
    e match {
      case CONST(a) => { CONST(0) }
      case VAR(a) => { if (a == v) CONST(1) else CONST(0) }
      case POWER(a, n) => {
        
          if (
            a == v
          ) {
            if (n == 0) CONST(0) else TIMES(List(CONST(n), POWER(a, n - 1))) 
          } else {
            CONST(0)
          }
      }
      case TIMES(l) => {
        l match {
          case Nil() => { CONST(1) }
          case Cons(h, t) => {
            
              if (
                diff(h, v) == CONST(0)
              ) {
                TIMES(List(h, diff(TIMES(t), v))) 
              } else if (
                diff(h, v) == CONST(1)
              ) {
                SUM(List(TIMES(t), TIMES(List(h, diff(TIMES(t), v))))) 
              } else {
                SUM(
                  List(TIMES(List(diff(h, v)) ++ t),
                   TIMES(List(h, diff(TIMES(t), v)))))
              }
          }
        }
      }
      case SUM(l) => {
        l match {
          case Nil() => { CONST(0) }
          case Cons(h, t) => {
            
              if (
                diff(h, v) == CONST(0)
              ) {
                diff(SUM(t), v) 
              } else {
                SUM(List(diff(h, v), diff(SUM(t), v)))
              }
          }
        }
      }
    }
  }
}