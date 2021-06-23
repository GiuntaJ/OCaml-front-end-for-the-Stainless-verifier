import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub106 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((aein, str))) = {
    aein match {
      case CONST(n) => { CONST(0) }
      case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
      case POWER(s, n) => {
        
          if (
            s == str
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
                TIMES(List(CONST(n), POWER(s, n - 1)))
              } 
          } else {
            CONST(0)
          }
      }
      case TIMES(Nil()) => { assert(false, "InvalidArgument") }
      case SUM(Nil()) => { assert(false, "InvalidArgument") }
      case TIMES(Cons(hd, tl)) => {
        
          if (
            tl == Nil()
          ) {
            diff(hd, str) 
          } else {
            SUM(
              List(TIMES(diff(hd, str) :: tl),
               TIMES(List(hd, diff(TIMES(tl), str)))))
          }
      }
      case SUM(Cons(hd, tl)) => {
        
          if (
            tl == Nil()
          ) {
            diff(hd, str) 
          } else {
            SUM(List(diff(hd, str), diff(SUM(tl), str)))
          }
      }
    }
  }
}