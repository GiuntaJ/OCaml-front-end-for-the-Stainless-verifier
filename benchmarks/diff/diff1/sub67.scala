import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sub67 {
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  sealed case class InvalidArgument() extends Exception {}
  
  def diff(((ae, s))) = {
    (ae, s) match {
      case (SUM(Nil()), s) => { assert(false, "InvalidArgument") }
      case (TIMES(Nil()), s) => { assert(false, "InvalidArgument") }
      case (CONST(i), s) => { CONST(0) }
      case (VAR(s1), s) => { if (s1 == s) CONST(1) else CONST(0) }
      case (POWER(s1, a), s) => {
        
          if (
            s1 == s
          ) {
            TIMES(List(CONST(a), POWER(s1, a - 1))) 
          } else {
            TIMES(List(CONST(a), diff(VAR(s1), s), POWER(s1, a - 1)))
          }
      }
      case (SUM(Cons(a, Cons(b, Nil()))), s) => {
        SUM(List(diff(a, s), diff(b, s)))
      }
      case (SUM(Cons(a, sl)), s) => { SUM(List(diff(a, s), diff(SUM(sl), s))) }
      case (TIMES(Cons(a, Cons(b, Nil()))), s) => {
        SUM(List(TIMES(List(diff(a, s), b)), TIMES(List(a, diff(b, s)))))
      }
      case (TIMES(Cons(a, sl)), s) => {
        SUM(List(TIMES(diff(a, s) :: sl), TIMES(List(a, diff(TIMES(sl), s)))))
      }
    }
  }
}