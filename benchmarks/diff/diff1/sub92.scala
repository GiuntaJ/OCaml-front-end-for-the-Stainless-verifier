import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub92 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((exp, diffwith))) = {
    exp match {
      case CONST(num) => { CONST(0) }
      case VAR(var0) => { if (var0 == diffwith) CONST(1) else CONST(0) }
      case POWER(var0, num) => {
        
          if (
            var0 == diffwith && num == 0
          ) {
            CONST(0) 
          } else if (
            var0 == diffwith && num ne 0
          ) {
            TIMES(List(CONST(num), POWER(var0, num - 1))) 
          } else {
            CONST(0)
          }
      }
      case TIMES(aelist) => {
        
          if (
            aelist.length > 1
          ) {
            SUM(
              List(TIMES(List(diff(aelist.head, diffwith)) ++(aelist.tail)),
               TIMES(List(aelist.head, diff(TIMES(aelist.tail), diffwith))))) 
          } else if (
            aelist.length == 0
          ) {
            assert(false, "InvalidArgument") 
          } else {
            diff(aelist.head, diffwith)
          }
      }
      case SUM(aelist) => {
        
          if (
            aelist.length > 1
          ) {
            SUM(
              List(diff(aelist.head, diffwith),
               diff(SUM(aelist.tail), diffwith))) 
          } else if (
            aelist.length == 0
          ) {
            assert(false, "InvalidArgument") 
          } else {
            diff(aelist.head, diffwith)
          }
      }
    }
  }
}