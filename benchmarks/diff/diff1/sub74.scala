import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub74 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((ae, str))) = {
    ae match {
      case CONST(i) => { CONST(0) }
      case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
      case POWER(s, i) => {
        
          if (
            s == str
          ) {
            TIMES(List(CONST(i)) ++ List(POWER(s, i - 1))) 
          } else {
            CONST(0)
          }
      }
      case TIMES(Cons(hd, tl)) => {
        
          if (
            tl == Nil()
          ) {
            diff(hd, str) 
          } else {
            SUM(
              List(TIMES(List(diff(hd, str)) ++ tl)) ++
              List(TIMES(List(hd) ++ List(diff(TIMES(tl), str)))))
          }
      }
      case TIMES(Nil()) => { assert(false, "InvalidArgument") }
      case SUM(Cons(hd, tl)) => {
        
          if (
            tl == Nil()
          ) {
            diff(hd, str) 
          } else {
            SUM(List(diff(hd, str)) ++ List(diff(SUM(tl), str)))
          }
      }
      case SUM(Nil()) => { assert(false, "InvalidArgument") }
    }
  }
}