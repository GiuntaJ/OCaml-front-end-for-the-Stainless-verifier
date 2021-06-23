import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub85 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((expr, var0))) = {
    expr match {
      case CONST(x) => { CONST(0) }
      case VAR(x) => { if (x == var0) CONST(1) else CONST(0) }
      case POWER(x, pow) => {
        
          if (
            x == var0
          ) {
            
              if (
                pow ne 0
              ) {
                TIMES(List(CONST(pow), POWER(x, pow - 1))) 
              } else {
                CONST(0)
              } 
          } else {
            CONST(0)
          }
      }
      case TIMES(xlist) => {
        
          if (
            xlist == Nil()
          ) {
            assert(false, "InvalidArgument") 
          } else if (
            xlist.length == 1
          ) {
            diff(xlist.head, var0) 
          } else {
            SUM(
              List(TIMES(List(diff(xlist.head, var0), TIMES(xlist.tail))),
               TIMES(List(xlist.head, diff(TIMES(xlist.tail), var0)))))
          }
      }
      case SUM(xlist) => {
        
          if (
            xlist == Nil()
          ) {
            assert(false, "InvalidArgument") 
          } else if (
            xlist.length == 1
          ) {
            diff(xlist.head, var0) 
          } else {
            SUM(List(diff(xlist.head, var0), diff(SUM(xlist.tail), var0)))
          }
      }
    }
  }
}
