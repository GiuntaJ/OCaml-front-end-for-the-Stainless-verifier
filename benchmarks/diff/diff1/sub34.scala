import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub34 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((ae, str))) = {
    ae match {
      case CONST(n) => { CONST(0) }
      case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
      case POWER(s, i) => {
        if (s == str) TIMES(List(CONST(i), POWER(s, i - 1))) else CONST(0)
      }
      case TIMES(Cons(hd, Nil())) => { diff(hd, str) }
      case TIMES(Cons(hd, tl)) => {
        SUM(
          List(TIMES(List(diff(hd, str), TIMES(tl))),
           TIMES(List(hd, diff(TIMES(tl), str)))))
      }
      case SUM(Cons(hd, Nil())) => { diff(hd, str) }
      case SUM(Cons(hd, tl)) => { SUM(List(diff(hd, str), diff(SUM(tl), str))) }
    }
  }
}