import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub103 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  sealed case class InvalidArgument() extends Exception {}
  def diff(((ae, s))) = {
    ae match {
      case CONST(n) => { CONST(0) }
      case VAR(x) => { if (s == x) CONST(1) else CONST(0) }
      case POWER(x, n) => {
        
          if (
            s == x
          ) {
            
              if (
                n eq 0
              ) {
                CONST(0) 
              } else if (
                n eq 1
              ) {
                CONST(1) 
              } else {
                TIMES(List(CONST(n), POWER(x, n - 1)))
              } 
          } else {
            POWER(x, n)
          }
      }
      case TIMES(ael) => {
        
          if (
            ael.length eq 0
          ) {
            assert(false, "InvalidArgument") 
          } else if (
            ael.length eq 1
          ) {
            diff(ael.head, s) 
          } else {
            SUM(
              List(TIMES(diff(ael.head, s) :: ael.tail),
               TIMES(List(ael.head, diff(TIMES(ael.tail), s)))))
          }
      }
      case SUM(ael) => {
        
          if (
            ael.length eq 0
          ) {
            assert(false, "InvalidArgument") 
          } else if (
            ael.length eq 1
          ) {
            diff(ael.head, s) 
          } else {
            SUM(List(diff(ael.head, s), diff(SUM(ael.tail), s)))
          }
      }
    }
  }
}