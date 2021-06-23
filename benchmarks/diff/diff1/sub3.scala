import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub3 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((a: Ae, s: String))) = {
    a match {
      case CONST(i) => { CONST(0) }
      case VAR(v) => { if (v == s) CONST(1) else CONST(0) }
      case POWER(v, p) => {
        if (v == s) TIMES(List(CONST(p), POWER(v, p - 1))) else CONST(0)
      }
      case TIMES(l) => {
        SUM(
          l.map(( (x) => { TIMES(diff(x, s) :: l.filter(( (y) => { y ne x } ))) } )))
      }
      case SUM(l) => { SUM(l.map(( (x) => { diff(x, s) } ))) }
    }
  }
}
