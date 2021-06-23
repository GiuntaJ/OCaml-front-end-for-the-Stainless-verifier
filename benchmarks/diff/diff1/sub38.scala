import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub38 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((exp, var0))) = {
    exp match {
      case CONST(_) => { CONST(0) }
      case VAR(str) => { if (str == var0) CONST(1) else CONST(0) }
      case POWER(str, n) => {
        if (str == var0) TIMES(List(CONST(n), POWER(str, n - 1))) else CONST(0)
      }
      case TIMES(Nil()) | SUM(Nil()) => { CONST(0) }
      case TIMES(Cons(hd, tl)) => {
        SUM(
          List(TIMES(diff(hd, var0) :: tl),
           TIMES(List(hd, diff(TIMES(tl), var0)))))
      }
      case SUM(Cons(hd, tl)) => { SUM(List(diff(hd, var0), diff(SUM(tl), var0)))
      }
    }
  }
  	
}