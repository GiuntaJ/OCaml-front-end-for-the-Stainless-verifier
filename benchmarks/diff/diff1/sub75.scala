import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub75 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((xp, str))) = {
    xp match {
      case CONST(_) => { CONST(0) }
      case VAR(str0) => { if (str == str0) CONST(1) else CONST(0) }
      case POWER(str0, n) => {
        if (str == str0) TIMES(List(CONST(n), POWER(str0, n - 1))) else CONST(0)
      }
      case TIMES(Nil()) => { assert(false, "InvalidArgument") }
      case TIMES(Cons(xp0, Nil())) => { diff(xp0, str) }
      case TIMES(xp_ls) => {
        SUM(
          List(TIMES(diff(xp_ls.head, str) :: xp_ls.tail),
           TIMES(List(xp_ls.head, diff(TIMES(xp_ls.tail), str)))))
      }
      case SUM(Nil()) => { assert(false, "InvalidArgument") }
      case SUM(Cons(xp0, Nil())) => { diff(xp0, str) }
      case SUM(xp_ls) => { SUM(xp_ls.map(( (_xp) => { diff(_xp, str) } ))) }
    }
  }
}