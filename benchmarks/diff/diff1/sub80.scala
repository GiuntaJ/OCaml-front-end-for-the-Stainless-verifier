import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub80 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((ae, str))) = {
    val _2 = {
      def strip(ae) = {
        ae match {
          case TIMES(Nil()) => { CONST(1) }
          case TIMES(Cons(CONST(0), tl)) => { CONST(0) }
          case TIMES(Cons(hd, Cons(CONST(0), Nil()))) => { CONST(0) }
          case TIMES(Cons(CONST(1), tl)) => { strip(TIMES(tl)) }
          case TIMES(Cons(hd, Cons(CONST(1), Nil()))) => { hd }
          case TIMES(Cons(hd, Nil())) => { hd }
          case SUM(Nil()) => { CONST(0) }
          case SUM(Cons(hd, Nil())) => { hd }
          case SUM(Cons(hd, Cons(CONST(0), Nil()))) => { hd }
          case SUM(Cons(CONST(0), tl)) => { strip(SUM(tl)) }
          case _ => { ae }
        }
      }
      ae match {
        case CONST(i) => { CONST(0) }
        case VAR(s) => { if (s == str) CONST(1) else CONST(0) }
        case POWER(s, i) => {
          
            if (
              s == str
            ) {
              strip(TIMES(List(CONST(i), POWER(s, i - 1)))) 
            } else {
              CONST(0)
            }
        }
        case TIMES(Nil()) => { assert(false, "InvalidArgument") }
        case TIMES(Cons(hd, tl)) => {
          strip(
            SUM(
              List(strip(TIMES(diff(hd, str) :: tl)),
               strip(TIMES(List(hd, diff(strip(TIMES(tl)), str)))))))
        }
        case SUM(Nil()) => { assert(false, "InvalidArgument") }
        case SUM(Cons(a, b)) => {
          strip(SUM(List(diff(a, str), diff(strip(SUM(b)), str))))
        }
      }
    }
  }
}