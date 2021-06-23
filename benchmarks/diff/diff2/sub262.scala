import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub262 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(s) => { if (s == x) Const(1) else Const(0) }
          case Power(s, n) => {
            if (s == x) Times(List(Const(n), Power(s, n - 1))) else Const(0)
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(h, t) => {
                Sum(
                  List(Times(List(diff(h, x)) ++ t),
                   Times(List(h, diff(Times(t), x)))))
              }
            }
          }
          case Sum(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(h, t) => { Sum(List(diff(h, x), diff(Sum(t), x))) }
            }
          }
        }
    }
  }
}