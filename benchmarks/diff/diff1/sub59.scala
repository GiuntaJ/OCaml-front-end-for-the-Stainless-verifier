import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub59 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((a, s))) = {
    a match {
      case CONST(c) => { CONST(0) }
      case VAR(c) => { if (c == s) CONST(1) else CONST(0) }
      case POWER(c, n) => {
        
          if (
            c == s
          ) {
            if (n ne 0) TIMES(List(CONST(n), POWER(c, n - 1))) else CONST(0) 
          } else {
            CONST(0)
          }
      }
      case TIMES(Nil()) => { assert(false, "InvalidArgument") }
      case TIMES(Cons(hd, Nil())) => { diff(hd, s) }
      case TIMES(Cons(hd, tl)) => {
        SUM(
          List(TIMES(List(diff(hd, s), TIMES(tl))),
           TIMES(List(hd, diff(TIMES(tl), s)))))
      }
      case SUM(Nil()) => { assert(false, "InvalidArgument") }
      case SUM(Cons(hd, Nil())) => { diff(hd, s) }
      case SUM(Cons(hd, tl)) => { SUM(List(diff(hd, s), diff(SUM(tl), s))) }
    }
  }
}