import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub27 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  
  def diff(((exp, str))) = {
    exp match {
      case CONST(num) => { CONST(0) }
      case VAR(st) => { if (st == str) CONST(1) else CONST(0) }
      case POWER(st, num) => {
        
          if (
            st == str
          ) {
            
              if (
                num == 1
              ) {
                CONST(num) 
              } else {
                TIMES(List(POWER(st, num - 1), CONST(num)))
              } 
          } else {
            CONST(0)
          }
      }
      case SUM(a) => { SUM(diffSum(a, str)) }
      case TIMES(a) => {
        a match {
          case Nil() => { CONST(0) }
          case Cons(b, Nil()) => { diff(b, str) }
          case Cons(b, bl) => {
            SUM(
              List(TIMES(diff(b, str) :: bl),
               TIMES(List(b, diff(TIMES(bl), str)))))
          }
        }
      }
    }
  }
  def diffSum(((explist, str))) = {
    explist match {
      case Nil() => { Nil() }
      case Cons(a, Nil()) => { List(diff(a, str)) }
      case Cons(a, al) => { diff(a, str) :: diffSum(al, str) }
    }
  }
}