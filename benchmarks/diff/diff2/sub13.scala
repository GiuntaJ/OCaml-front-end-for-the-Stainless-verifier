import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub13 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def diff: (Aexp, String) => Aexp = {
    case (aexp, x) =>
      {
        (aexp, x) match {
          case (Const(c), _) => { Const(0) }
          case (Var(a), t) => { if (a == t) Const(1) else Const(0) }
          case (Power(a, b), t) => {
            if (a == t) Times(List(Const(b), Power(a, b - 1))) else Const(0)
          }
          case (Times(lst), t) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(head, Nil()) => { diff(head, t) }
              case Cons(head, tail) => {
                Sum(
                  List(Times(List(diff(head, t)) ++ tail),
                   Times(List(head, diff(Times(tail), t)))))
              }
            }
          }
          case (Sum(lst), t) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(head, Nil()) => { diff(head, t) }
              case Cons(head, tail) => {
                Sum(List(diff(head, t)) ++ List(diff(Sum(tail), t)))
              }
            }
          }
        }
    }
  }
                    
}
