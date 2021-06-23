import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub9 {
  /* 20008-11874 SUJEE LEE execise 2 */
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  
  def diff(((ae, str))) = {
    ae match {
      case CONST(c) => { CONST(0) }
      case VAR(v) => { if (str == v) CONST(1) else CONST(0) }
      case POWER(v, p) => {
        if (v == str) TIMES(List(CONST(p), POWER(v, p - 1))) else CONST(0)
      }
      case TIMES(ael) => {
        ael match {
          case Nil() => { TIMES(Nil()) }
          case Cons(hd, Nil()) => { diff(hd, str) }
          case Cons(hd, tl) => {
            SUM(
              List(TIMES(diff(hd, str) :: tl),
               TIMES(List(hd, diff(TIMES(tl), str)))))
          }
        }
      }
      case SUM(ael) => {
        val _2 = {
          def diffwithstr(e) = { diff(e, str) }
          SUM(ael.map(diffwithstr))
        }
      }
    }
  }
}