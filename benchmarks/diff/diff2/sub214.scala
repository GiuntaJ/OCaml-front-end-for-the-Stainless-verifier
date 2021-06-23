import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub214 {
  /* problem 4*/ 
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
          case Const(b) => { Const(0) }
          case Var(a) => { if (a == x) Const(1) else Var(a) }
          case Power(a, b) => {
            if (a == x) Times(List(Const(b), Power(a, b - 1))) else Power(a, b)
          }
          case Times(l) => {
            l match {
              case Nil() => { Times(l) }
              case Cons(head, tail) => {
                Sum(
                  List(Times(List(diff(head, x)) ++ tail),
                   Times(List(head, diff(Times(tail), x)))))
              }
            }
          }
          case Sum(l2) => {
            l2 match {
              case Nil() => { Sum(l2) }
              case Cons(head, tail) => {
                Sum(List(diff(head, x), diff(Sum(tail), x)))
              }
            }
          }
        }
    }
  }
}