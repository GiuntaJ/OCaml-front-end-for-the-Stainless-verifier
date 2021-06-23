import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub11 {
  /* Ex 2 */
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class ILLEGAL_EXPRESSION(param0: String) extends Exception {} 
  def diff(((ae: Ae, str: String))) = {
    ae match {
      case CONST(_) => { CONST(0) }
      case VAR(x) => { if (x == str) CONST(1) else CONST(0) }
      case POWER(base, expnt) => {
        
          if (
            base == str
          ) {
            TIMES(List(CONST(expnt), POWER(base, expnt - 1))) 
          } else {
            CONST(0)
          }
      }
      case TIMES(Nil()) => {
        assert(
          false,
          "ILLEGAL_EXPRESSION with         The argument of TIMES is illegal")
      }
      case TIMES(Cons(hd, Nil())) => { diff(hd, str) }
      case TIMES(Cons(hd, tl)) => {
        SUM(
          List(TIMES(List(diff(hd, str)) ++ tl)) ++
          List(TIMES(List(hd, diff(TIMES(tl), str)))))
      }
      case SUM(Nil()) => {
        assert(false, "ILLEGAL_EXPRESSION with The argument of SUM is illega")
      }
      case SUM(Cons(hd, Nil())) => { diff(hd, str) }
      case SUM(Cons(hd, tl)) => {
        SUM(List(diff(hd, str)) ++ List(diff(SUM(tl), str)))
      }
    }
  }
}