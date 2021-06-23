import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub58 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((e, str))) = {
    e match {
      case CONST(i) => { CONST(0) }
      case VAR(str1) => { if (str1 == str) CONST(1) else CONST(0) }
      case POWER(str1, i) => {
        
          if (
            i == 0
          ) {
            CONST(0) 
          } else if (
            str1 == str && i == 1
          ) {
            CONST(1) 
          } else {
            TIMES(List(CONST(i), POWER(str1, i - 1)))
          }
      }
      case SUM(Nil()) => { assert(false, "InvalidArgument") }
      case SUM(lst) => {
        
          if (
            lst.length == 1
          ) {
            diff(lst.head, str) 
          } else {
            SUM(List(diff(lst.head, str), diff(SUM(lst.tail), str)))
          }
      }
      case TIMES(Nil()) => { assert(false, "InvalidArgument") }
      case TIMES(lst) => {
        
          if (
            lst.length == 1
          ) {
            diff(lst.head, str) 
          } else {
            SUM(
              List(TIMES(diff(lst.head, str) :: lst.tail),
               TIMES(List(lst.head, diff(TIMES(lst.tail), str)))))
          }
      }
    }
  }
  				       
  				
}