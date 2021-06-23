import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub5 {
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  
  def diff(((ae, x))) = {
    ae match {
      case CONST(n) => { CONST(0) }
      case VAR(v) => { if (v == x) CONST(1) else CONST(0) }
      case POWER(v, n) => {
        if (v == x) TIMES(List(CONST(n), POWER(v, n - 1))) else CONST(0)
      }
      case SUM(l) => {
        val _2 = {
          def lstRec2(l) = {
            l match {
              case Nil() => { CONST(0) }
              case Cons(h, t) => { SUM(List(diff(h, x), lstRec2(t))) }
            }
          }
          lstRec2(l)
        }
      }
    }
  }
}