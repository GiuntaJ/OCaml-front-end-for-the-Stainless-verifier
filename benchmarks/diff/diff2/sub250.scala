import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub250 {
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
          case Const(a) => { Const(0) }
          case Var(s) => { if (s == x) Const(1) else Var(s) }
          case Power(s, i) => {
            if (s == x) Times(List(Const(i), Power(s, i - 1))) else Power(s, i)
          }
          case Times(l) => {
            l match {
              case Cons(h1, Cons(h2, Cons(h3, Nil()))) => {
                Sum(
                  List(Times(List(diff(h1, x), h2, h3)), diff(h2, x),
                   diff(h3, x)))
              }
              case Cons(h1, Cons(h2, Nil())) => {
                Sum(List(Times(List(diff(h1, x), h2)), diff(h2, x)))
              }
              case Cons(h1, Nil()) => { Sum(List(Times(List(diff(h1, x))))) }
            }
          }
          case Sum(m) => {
            m match {
              case Cons(h1, Cons(h2, Cons(h3, Cons(h4, t)))) => {
                Sum(List(diff(h1, x), diff(h2, x), diff(h3, x), diff(h4, x)))
              }
              case Cons(h1, Cons(h2, Cons(h3, t))) => {
                Sum(List(diff(h1, x), diff(h2, x), diff(h3, x)))
              }
              case Cons(h1, Cons(h2, t)) => {
                Sum(List(diff(h1, x), diff(h2, x)))
              }
              case Cons(h1, t) => { Sum(List(diff(h1, x))) }
              case Nil() => { Sum(Nil()) }
            }
          }
        }
    }
  }
    
    diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
}