import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub45 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((exp, s))) = {
    exp match {
      case CONST(i) => { CONST(0) }
      case VAR(a) => { if (a == s) CONST(1) else CONST(0) }
      case POWER(a, i) => {
        
          if (
            a == s
          ) {
            
              if (
                i == 0
              ) {
                CONST(0) 
              } else if (
                i == 1
              ) {
                CONST(1) 
              } else {
                TIMES(List(CONST(i), POWER(a, i - 1)))
              } 
          } else {
            CONST(0)
          }
      }
      case SUM(lst) => {
        lst match {
          case Nil() => { CONST(0) }
          case Cons(hd, Nil()) => { diff(hd, s) }
          case Cons(hd, tl) => { SUM(List(diff(hd, s), diff(SUM(tl), s))) }
        }
      }
      case TIMES(lst) => {
        lst match {
          case Nil() => { CONST(0) }
          case _ => { SUM(tCheck(lst.reverse, lst.length - 1, s)) }
        }
      }
    }
  }
  def partT(((lst, n, s))) = {
    lst match {
      case Nil() => { Nil() }
      case Cons(hd, tl) => {
        if (n == 0) diff(hd, s) :: tl else hd :: partT(tl, n - 1, s)
      }
    }
  }
  def tCheck(((lst, n, s))) = {
    lst match {
      case Nil() => { Nil() }
      case _ => {
        
          if (
            n == 0
          ) {
            List(TIMES(partT(lst, 0, s))) 
          } else {
            TIMES(partT(lst, n, s)) :: tCheck(lst, n - 1, s)
          }
      }
    }
  }
  			
  /*			match hd with
  			| CONST x -> diff(tl, s)
  			| VAL x -> diff(hd, s)::diff(tl, s)
  			| POWER x -> diff(hd, s)::diff(tl, s)
  			| TIMES x -> diff(hd, s)@diff(tl, s)
  			| SUM x -> diff(hd, s)@diff(tl, s)
  */
}