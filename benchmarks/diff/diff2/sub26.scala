import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub26 {
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
          case Var(a) => { Var(a) }
          case Power(a, b) => {
            if (b == 1) Var(a) else Times(List(Const(b), Power(a, b - 1)))
          }
          case Times(a) => {
            a match {
              case Cons(hd, tl) => {
                hd match {
                  case Const(t) => {
                    Times(
                      List(Const(t),
                       Sum(
                         List(Times(List(Const(1))),
                          Times(List(Var(x), Const(0)))))))
                  }
                  case Var(t) => { Times(List(Var(t), Const(0))) }
                }
              }
            }
          }
          case Sum(a) => {
            a match {
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
              case _ => { Const(0) }
            }
          }
        }
    }
  }
}