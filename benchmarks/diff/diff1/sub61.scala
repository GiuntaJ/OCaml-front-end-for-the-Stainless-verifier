import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub61 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((ae, s))) = {
    val _2 = {
      def diff_times(s, l, r) = {
        r match {
          case Cons(hd, tl) => {
            List(TIMES(l ++ List(diff(hd, s)) ++ tl)) ++
            diff_times(s, l ++ List(hd), tl)
          }
          case Nil() => { Nil() }
        }
      }
      val _3 = {
        def diff_sub(s, ae) = {
          ae match {
            case CONST(_) => { CONST(0) }
            case VAR(x) => { if (x == s) CONST(1) else CONST(0) }
            case POWER(x, n) => {
              
                if (
                  x != s
                ) {
                  CONST(0) 
                } else if (
                  n == 0
                ) {
                  CONST(0) 
                } else if (
                  n == 1
                ) {
                  CONST(1) 
                } else {
                  TIMES(List(CONST(n), POWER(x, n - 1)))
                }
            }
            case SUM(l) => {
              
                if (
                  l == Nil()
                ) {
                  assert(false, "InvalidArgument") 
                } else {
                  SUM(l.map(diff_sub(s)))
                }
            }
            case TIMES(l) => {
              
                if (
                  l == Nil()
                ) {
                  assert(false, "InvalidArgument") 
                } else {
                  SUM(diff_times(s, Nil(), l))
                }
            }
          }
        }
        diff_sub(s, ae)
      }
    }
  }
}