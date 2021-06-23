import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub39 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((exp, var0))) = {
    val _2 = {
      def diff2(((exp, var0))) = {
        exp match {
          case CONST(i) => { CONST(0) }
          case VAR(s) => { if (s == var0) CONST(1) else CONST(0) }
          case POWER(s, i) => {
            if (s == var0) TIMES(List(CONST(i), POWER(s, i - 1))) else CONST(0)
          }
          case TIMES(Cons(h, t)) => {
            SUM(
              List(TIMES(List(diff2(h, var0), TIMES(t))),
               TIMES(List(h, diff2(TIMES(t), var0)))))
          }
          case TIMES(Nil()) => { CONST(0) }
          case SUM(Cons(h, t)) => {
            SUM(List(diff2(h, var0), diff2(SUM(t), var0)))
          }
          case SUM(Nil()) => { CONST(0) }
        }
      }
      val _3 = {
        def reduce(exp) = {
          exp match {
            case _ => { exp }
          }
        }
        reduce(diff2(exp, var0))
      }
    }
  }
  
      /*
  ;;
  
  diff (SUM[TIMES[VAR "a"; POWER("x", 2)]; TIMES[VAR("b"); VAR("x")]; VAR("c")], "x");;
  
  diff (CONST 2, "x");;
  diff (VAR "y", "y");;
  diff (POWER("y", 2), "y");;
  diff (POWER("y", 0), "y");;
  diff (POWER("y", 1), "y");;
  */
}