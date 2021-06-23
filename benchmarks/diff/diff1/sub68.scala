import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub68 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((ae, v))) = {
    val _2 = {
      def aux(((ae, v))) = {
        ae match {
          case CONST(c) => { CONST(0) }
          case VAR(x) => { if (x == v) CONST(1) else CONST(0) }
          case POWER(x, n) => {
            if (x == v) TIMES(List(CONST(n), POWER(x, n - 1))) else CONST(0)
          }
          case TIMES(Nil()) => { CONST(0) }
          case TIMES(Cons(CONST(0), tl)) => { CONST(0) }
          case TIMES(Cons(hd, tl)) => {
            SUM(
              List(TIMES(aux(hd, v) :: tl), TIMES(List(hd, aux(TIMES(tl), v)))))
          }
          case SUM(aelist) => { SUM(aelist.map(( (ae) => { aux(ae, v) } ))) }
        }
      }
      ae match {
        case TIMES(Nil()) => { assert(false, "InvalidArgument") }
        case SUM(Nil()) => { assert(false, "InvalidArgument") }
        case _ => { aux(ae, v) }
      }
    }
  }
}