import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub36 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def diff(((exp, s))) = {
    val _2 = {
      def mul(((a, b))) = {
        b match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => {
            TIMES(diff(hd, s) :: a ++(tl)) :: mul(hd :: a, tl)
          }
        }
      }
      val _3 = {
        def hap(a) = {
          a match {
            case Nil() => { Nil() }
            case Cons(hd, tl) => { diff(hd, s) :: hap(tl) }
          }
        }
        exp match {
          case CONST(a) => { CONST(0) }
          case VAR(a) => { if (a == s) CONST(1) else CONST(0) }
          case POWER(a, b) => {
            if (a == s) TIMES(List(CONST(b), POWER(a, b - 1))) else CONST(0)
          }
          case TIMES(a) => { SUM(mul(Nil(), a)) }
          case SUM(a) => { SUM(hap(a)) }
        }
      }
    }
  }
}