import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub88 {
  /*
  	department : computer science & engineering
  	student ID : 2012-11242 / name : Seon-bi, Park
  */
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  sealed case class InvalidArgument() extends Exception {}
  
  def cal(((form, var0))) = {
    form match {
      case CONST(n) => { CONST(0) }
      case VAR(x) => { if (x == var0) CONST(1) else CONST(0) }
      case POWER(x, n) => {
        
          if (
            x == var0
          ) {
            
              if (
                n == 0
              ) {
                CONST(0) 
              } else if (
                n == 1
              ) {
                CONST(1) 
              } else {
                TIMES(List(CONST(n), POWER(x, n - 1)))
              } 
          } else {
            CONST(0)
          }
      }
      case TIMES(lst) => {
        
          if (
            lst.contains(CONST(0))
          ) {
            CONST(0) 
          } else {
            lst match {
              case Nil() => { assert(false, "InvalidArgument") }
              case Cons(l, Nil()) => { cal(l, var0) }
              case Cons(l, r) => {
                l match {
                  case CONST(1) => { cal(TIMES(r), var0) }
                  case CONST(n) => { TIMES(List(l, cal(TIMES(r), var0))) }
                  case _ => {
                    SUM(
                      List(TIMES(cal(l, var0) :: r),
                       TIMES(List(l, cal(TIMES(r), var0)))))
                  }
                }
              }
            }
          }
      }
      case SUM(lst) => {
        lst match {
          case Nil() => { assert(false, "InvalidArgument") }
          case Cons(l, Nil()) => { cal(l, var0) }
          case Cons(l, r) => { SUM(List(cal(l, var0), cal(SUM(r), var0))) }
        }
      }
    }
  }
  
  def notone(x: Ae): Boolean = { if (x == CONST(1)) false else true }
  def notzero(x: Ae): Boolean = { if (x == CONST(0)) false else true }
  
  def arrange(form: Ae): Ae = {
    form match {
      case TIMES(lst) => {
        
          if (
            lst.filter(notone) == Nil()
          ) {
            CONST(1) 
          } else {
            TIMES(CONST(1) :: lst.filter(notone))
          }
      }
      case SUM(lst) => {
        
          if (
            lst.filter(notzero) == Nil()
          ) {
            CONST(0) 
          } else {
            SUM(CONST(0) :: lst.filter(notzero))
          }
      }
      case _ => { form }
    }
  }
  
  def diff(((form, var0))) = { arrange(cal(form, var0)) }
}