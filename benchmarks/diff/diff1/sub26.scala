import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub26 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((ae, str))) = {
    (ae, str) match {
      case (CONST(a), str) => { CONST(0) }
      case (VAR(a), str) => { if (a == str) CONST(1) else CONST(0) }
      case (POWER(a, n), str) => {
        if (a == str) TIMES(List(CONST(n), POWER(a, n - 1))) else CONST(0)
      }
      case (TIMES(Cons(hd, Nil())), str) => { diff(hd, str) }
      case (TIMES(Cons(hd, tl)), str) => {
        SUM(
          List(TIMES(diff(hd, str) :: tl),
           TIMES(List(hd, diff(TIMES(tl), str)))))
      }
      case (SUM(Cons(hd, Nil())), str) => { diff(hd, str) }
      case (SUM(Cons(hd, tl)), str) => {
        SUM(List(diff(hd, str), diff(SUM(tl), str)))
      }
    }
  }
}