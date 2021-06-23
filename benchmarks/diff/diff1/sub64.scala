import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub64 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  def diff(((a, stri))) = {
    a match {
      case CONST(na) => { CONST(0) }
      case VAR(strin) => { if (stri == strin) CONST(1) else CONST(0) }
      case POWER(strin, na) => {
        
          if (
            not(strin == stri)
          ) {
            CONST(0) 
          } else if (
            na == 0
          ) {
            CONST(0) 
          } else if (
            na == 1
          ) {
            CONST(1) 
          } else if (
            na == 2
          ) {
            TIMES(List(CONST(2), VAR(strin))) 
          } else {
            TIMES(List(CONST(na), POWER(strin, na - 1)))
          }
      }
      case TIMES(Cons(CONST(na), tl)) => {
        
          if (
            na == 0
          ) {
            CONST(0) 
          } else if (
            na == 1
          ) {
            diff(TIMES(tl), stri) 
          } else {
            TIMES(List(CONST(na), diff(TIMES(tl), stri)))
          }
      }
      case TIMES(Cons(hd, tl)) => {
        
          if (
            tl == Nil()
          ) {
            diff(hd, stri) 
          } else {
            SUM(
              List(TIMES(diff(hd, stri) :: tl),
               TIMES(List(hd, diff(TIMES(tl), stri)))))
          }
      }
      case TIMES(Nil()) => { assert(false, "InvalidArgument") }
      case SUM(Cons(CONST(na), tl)) => { diff(SUM(tl), stri) }
      case SUM(Cons(hd, tl)) => {
        
          if (
            tl == Nil()
          ) {
            diff(hd, stri) 
          } else {
            SUM(List(diff(hd, stri), diff(SUM(tl), stri)))
          }
      }
      case SUM(Nil()) => { assert(false, "InvalidArgument") }
    }
  }
}