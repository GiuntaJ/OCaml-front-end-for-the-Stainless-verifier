import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub102 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}  
    def diff_temp(((ae, str))) = {
    ae match {
      case CONST(n) => { CONST(0) }
      case VAR(str1) => { if (str1 == str) CONST(1) else CONST(0) }
      case POWER(str1, n) => {
        if (str1 == str) TIMES(List(CONST(n), POWER(str1, n - 1))) else CONST(0)
      }
      case TIMES(aelist) => {
        
          if (
            aelist.tail == Nil()
          ) {
            diff_temp(aelist.head, str) 
          } else {
            SUM(
              List(TIMES(List(diff_temp(aelist.head, str), TIMES(aelist.tail))),
               TIMES(List(aelist.head, diff_temp(TIMES(aelist.tail), str)))))
          }
      }
      case SUM(aelist) => {
        
          if (
            aelist.tail == Nil()
          ) {
            diff_temp(aelist.head, str) 
          } else {
            SUM(
              List(diff_temp(aelist.head, str),
               diff_temp(SUM(aelist.tail), str)))
          }
      }
    }
  }
    
    def diff(((ae, str))) = {
    ae match {
      case CONST(n) => { CONST(0) }
      case VAR(str1) => { if (str1 == str) CONST(1) else CONST(0) }
      case POWER(str1, n) => {
        if (str1 == str) TIMES(List(CONST(n), POWER(str1, n - 1))) else CONST(0)
      }
      case TIMES(aelist) => {
        
          if (
            aelist == Nil()
          ) {
            assert(false, "InvalidArgument") 
          } else {
            SUM(
              List(TIMES(List(diff_temp(aelist.head, str), TIMES(aelist.tail))),
               TIMES(List(aelist.head, diff_temp(TIMES(aelist.tail), str)))))
          }
      }
      case SUM(aelist) => {
        
          if (
            aelist == Nil()
          ) {
            assert(false, "InvalidArgument") 
          } else {
            SUM(
              List(diff_temp(aelist.head, str),
               diff_temp(SUM(aelist.tail), str)))
          }
      }
    }
  }
}