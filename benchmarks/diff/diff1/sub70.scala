import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub70 {
  sealed case class InvalidArgument() extends Exception {}
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def r_diff(((ae, str))) = {
    ae match {
      case CONST(i) => { CONST(0) }
      case VAR(v) => { if (v == str) CONST(1) else CONST(0) }
      case POWER(v, i) => {
        
          if (
            not(v == str)
          ) {
            CONST(0) 
          } else if (
            i == 0
          ) {
            CONST(0) 
          } else if (
            i == 1
          ) {
            CONST(1) 
          } else if (
            i == 2
          ) {
            TIMES(List(CONST(i), VAR(v))) 
          } else {
            TIMES(List(CONST(i), POWER(v, i - 1)))
          }
      }
      case TIMES(Cons(hd, tl)) => {
        
          if (
            tl == Nil()
          ) {
            r_diff(hd, str) 
          } else {
            SUM(
              List(TIMES(r_diff(hd, str) :: tl),
               TIMES(List(hd, r_diff(TIMES(tl), str)))))
          }
      }
      case TIMES(Nil()) => { assert(false, "InvalidArgument") }
      case SUM(Cons(hd, tl)) => {
        
          if (
            tl == Nil()
          ) {
            r_diff(hd, str) 
          } else {
            SUM(List(r_diff(hd, str), r_diff(SUM(tl), str)))
          }
      }
      case SUM(Nil()) => { assert(false, "InvalidArgument") }
    }
  }
  
  def contain_zero(lst: List[Ae]): Boolean = {
    lst match {
      case Nil() => { false }
      case Cons(hd, tl) => { if (hd == CONST(0)) true else contain_zero(tl) }
    }
  }
  
  def minimize_sum(lst: List[Ae]): List[Ae] = {
    lst match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => {
        hd match {
          case CONST(0) => { minimize_sum(tl) }
          case SUM(lst) => { lst ++ minimize_sum(tl) }
          case _ => { hd :: minimize_sum(tl) }
        }
      }
    }
  }
  
  def minimize_times(lst: List[Ae]): List[Ae] = {
    lst match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => {
        hd match {
          case CONST(1) => { minimize_times(tl) }
          case TIMES(lst) => { lst ++ minimize_times(tl) }
          case _ => { hd :: minimize_times(tl) }
        }
      }
    }
  }
  
  
  
  def minimize(ae: Ae): Ae = {
    ae match {
      case CONST(i) => { CONST(i) }
      case VAR(v) => { VAR(v) }
      case POWER(v, i) => { POWER(v, i) }
      case SUM(lst) => {
        
          if (
            lst == Nil()
          ) {
            CONST(0) 
          } else if (
            lst.length == 1
          ) {
            lst.head 
          } else {
            SUM(minimize_sum(lst).map(minimize))
          }
      }
      case TIMES(lst) => {
        
          if (
            contain_zero(lst)
          ) {
            CONST(0) 
          } else if (
            lst.length == 1
          ) {
            lst.head 
          } else {
            TIMES(minimize_times(lst).map(minimize))
          }
      }
    }
  }
  
  def r_minimize(ae: Ae): Ae = {
    if (ae == minimize(ae)) ae else r_minimize(minimize(ae))
  }
  
  def diff(((ae, str))) = { r_minimize(r_diff(ae, str)) }
}