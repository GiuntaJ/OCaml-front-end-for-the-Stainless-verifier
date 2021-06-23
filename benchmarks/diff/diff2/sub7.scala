import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub7 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        aexp match {
          case Const(a) => { Const(0) }
          case Var(alpha) => { if (alpha == x) Const(1) else aexp }
          case Power(alpha, a) => {
            if (alpha == x) Times(List(Const(a), Power(alpha, a - 1))) else aexp
          }
          case Sum(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                tl match {
                  case Nil() => { diff(hd, x) }
                  case _ => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
                }
              }
            }
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(1) }
              case Cons(hd, tl) => {
                tl match {
                  case Nil() => { diff(hd, x) }
                  case _ => {
                    Sum(
                      List(Times(List(diff(hd, x), Times(tl))),
                       Times(List(hd, diff(Times(tl), x)))))
                  }
                }
              }
            }
          }
        }
    }
  }
}