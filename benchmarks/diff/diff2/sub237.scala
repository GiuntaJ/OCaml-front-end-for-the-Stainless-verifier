import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub237 {
  /*problem 4*/
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (e, x) =>
      {
        e match {
          case Const(n) => { Const(0) }
          case Var(x) => { Const(1) }
          case Power(x, n) => {
            n match {
              case 0 => { Const(0) }
              case 1 => { Const(1) }
              case _ => { Times(List(Const(n), Power(x, n - 1))) }
            }
          }
          case Times(Cons(hd, tl)) => {
            hd match {
              case Const(n) => {
                
                  if (
                    n == 0
                  ) {
                    Const(0) 
                  } else {
                    tl match {
                      case Nil() => { Const(0) }
                      case _ => { Times(List(Const(n), diff(Times(tl), x))) }
                    }
                  }
              }
              case Var(x) => {
                tl match {
                  case Nil() => { Const(1) }
                  case _ => { Times(List(Const(1), diff(Times(tl), x))) }
                }
              }
              case Power(x, n) => {
                tl match {
                  case Nil() => { diff(Power(x, n), x) }
                  case _ => {
                    Times(
                      List(Times(List(Const(n), Power(x, n - 1))),
                       diff(Times(tl), x)))
                  }
                }
              }
              case _ => { Times(List(diff(hd, x), diff(Times(tl), x))) }
            }
          }
          case Sum(Cons(hd, tl)) => {
            hd match {
              case Const(n) => {
                tl match {
                  case Nil() => { Const(0) }
                  case _ => { diff(Sum(tl), x) }
                }
              }
              case _ => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
            }
          }
          case _ => { Const(0) }
        }
    }
  } 
}