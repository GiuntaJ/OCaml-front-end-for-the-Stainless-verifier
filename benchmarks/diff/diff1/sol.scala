import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff1_sol {
  sealed case class Failure(param0: String) extends Exception {}
  
  sealed abstract class Ae {}
  case class CONST(param0: Int63) extends Ae {}
  case class VAR(param0: String) extends Ae {}
  case class POWER(param0: String,  param1: Int63) extends Ae {}
  case class TIMES(param0: List[Ae]) extends Ae {}
  case class SUM(param0: List[Ae]) extends Ae {}
  
  def map: ((Ae, String) => Ae, (List[Ae], String)) => List[Ae] = {
    case (f, (l, x)) =>
      {
        l match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => { f(hd, x) :: map(f, tl, x) }
        }
    }
  }
  
  def diff: (Ae, String) => Ae = {
    case (e, x) =>
      {
        e match {
          case CONST(n) => { CONST(0) }
          case VAR(a) => { if (a != x) CONST(0) else CONST(1) }
          case POWER(a, n) => {
            
              if (
                n < 0
              ) {
                assert(false, "Failure with Invalid ") 
              } else if (
                n == 0 || a != x
              ) {
                CONST(0) 
              } else {
                TIMES(List(CONST(n), POWER(a, n - 1)))
              }
          }
          case TIMES(l) => {
            l match {
              case Nil() => { assert(false, "Failure with Invalid") }
              case Cons(hd, Nil()) => { diff(hd, x) }
              case Cons(hd, tl) => {
                SUM(
                  List(TIMES(diff(hd, x) :: tl),
                   TIMES(List(hd, diff(TIMES(tl), x)))))
              }
            }
          }
          case SUM(l) => {
            l match {
              case Nil() => { assert(false, "Failure with Invalid") }
              case _ => { SUM(map(diff, l, x)) }
            }
          }
        }
    }
  }
}