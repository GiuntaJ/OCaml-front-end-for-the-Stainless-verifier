import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub79 {
  sealed case class InvalidArgument() extends Exception {}
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  
  
  def diffSub(ae: Ae, str: String): Ae = {
    val _2 = {
      def diff_times(lst, str) = {
        lst.length match {
          case 1 => { List(diffSub(lst.head, str)) }
          case _ => {
            List(TIMES(List(diffSub(lst.head, str)) ++ lst.tail)) ++
            List(TIMES(List(lst.head) ++ List(diffSub(TIMES(lst.tail), str))))
          }
        }
      }
      val _3 = {
        def diff_sum(lst, str) = {
          lst match {
            case Cons(hd, tl) => { List(diffSub(hd, str)) ++(diff_sum(tl, str))
            }
            case Nil() => { Nil() }
          }
        }
        ae match {
          case CONST(_) => { CONST(0) }
          case VAR(s) => { if (str == s) CONST(1) else CONST(0) }
          case POWER(s, i) => {
            if (str == s) TIMES(List(CONST(i), POWER(s, i - 1))) else CONST(0)
          }
          case TIMES(Nil()) => { assert(false, "InvalidArgument") }
          case SUM(Nil()) => { assert(false, "InvalidArgument") }
          case TIMES(l) => { SUM(diff_times(l, str)) }
          case SUM(l) => { SUM(diff_sum(l, str)) }
        }
      }
    }
  }
  
  def diff(((ae, str))) = { diffSub(ae, str) }
}
