import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub253 {
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
          case Var(a) => { if (a == x) Const(1) else Const(0) }
          case Power(s, i) => {
            if (s == x) Times(List(Const(i), Power(s, i - 1))) else Const(0)
          }
          case Times(l) => {
            l match {
              case Nil() => { Const(1) }
              case Cons(a, Nil()) => { diff(a, x) }
              case Cons(h, t) => { Times(List(h, diff(Times(t), x))) }
            }
          }
          case Sum(m) => {
            m match {
              case Nil() => { Const(0) }
              case Cons(a, Nil()) => { diff(a, x) }
              case Cons(h, t) => { Sum(List(diff(h, x), diff(Sum(t), x))) }
            }
          }
        }
    }
  }
        
        
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
}