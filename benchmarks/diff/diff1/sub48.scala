import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub48 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((ae, v))) = {
    ae match {
      case CONST(_) => { CONST(0) }
      case VAR(x) => { if (x == v) CONST(1) else CONST(0) }
      case POWER(base, expnt) => {
        
          if (
            base == v
          ) {
            TIMES(List(CONST(expnt), POWER(base, expnt - 1))) 
          } else {
            CONST(0)
          }
      }
      case TIMES(Nil()) => { assert(false, "InvalidArgument") }
      case TIMES(Cons(hd, Nil())) => { diff(hd, v) }
      case TIMES(Cons(hd, tl)) => {
        SUM(List(TIMES(diff(hd, v) :: tl), TIMES(List(hd, diff(TIMES(tl), v)))))
      }
      case SUM(Nil()) => { assert(false, "InvalidArgument") }
      case SUM(Cons(hd, Nil())) => { diff(hd, v) }
      case SUM(Cons(hd, tl)) => { SUM(List(diff(hd, v), diff(SUM(tl), v))) }
    }
  }
     
}