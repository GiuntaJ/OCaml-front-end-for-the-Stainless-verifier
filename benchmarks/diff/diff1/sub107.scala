import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub107 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  sealed case class InvalidArgument() extends Exception {}
  def diff(((arg, var0))) = {
    arg match {
      case CONST(a) => { CONST(0) }
      case VAR(a) => { if (a == var0) CONST(1) else CONST(0) }
      case POWER(a, b) => {
        if (a == var0) TIMES(List(CONST(b), POWER(a, b - 1))) else CONST(0)
      }
      case TIMES(a) => {
        
          if (
            a.length == 0
          ) {
            assert(false, "InvalidArgument") 
          } else if (
            a.length == 1
          ) {
            diff(a.head, var0) 
          } else {
            SUM(
              List(TIMES(diff(a.head, var0) :: a.tail),
               TIMES(List(a.head, diff(TIMES(a.tail), var0)))))
          }
      }
      case SUM(a) => {
        
          if (
            a.length == 0
          ) {
            assert(false, "InvalidArgument") 
          } else if (
            a.length == 1
          ) {
            diff(a.head, var0) 
          } else {
            val _0 = {
              def diff2(x) = { diff(x, var0) }
              SUM(a.map(diff2))
            }
          }
      }
    }
  }
}