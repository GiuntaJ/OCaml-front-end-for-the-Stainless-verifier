import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub105 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((e, str))) = {
    e match {
      case CONST(_) => { CONST(0) }
      case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
      case POWER(s, n) => {
        if (s == str) TIMES(List(CONST(n), POWER(s, n - 1))) else CONST(0)
      }
      case TIMES(aelist) => {
        
          if (
            aelist == Nil()
          ) {
            assert(false, "InvalidArgument") 
          } else if (
            aelist.length == 1
          ) {
            diff(aelist.head, str) 
          } else {
            SUM(
              List(TIMES(diff(aelist.head, str) :: aelist.tail),
               TIMES(List(aelist.head, diff(TIMES(aelist.tail), str)))))
          }
      }
      case SUM(aelist) => {
        
          if (
            aelist == Nil()
          ) {
            assert(false, "InvalidArgument") 
          } else if (
            aelist.length == 1
          ) {
            diff(aelist.head, str) 
          } else {
            SUM(List(diff(aelist.head, str), diff(SUM(aelist.tail), str)))
          }
      }
    }
  }
}