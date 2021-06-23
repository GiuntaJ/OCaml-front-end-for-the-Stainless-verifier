import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub241 {
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
          case Var(str) => { if (str == x) Const(1) else Const(0) }
          case Power(str, n) => {
            if (str == x) Times(List(Const(n), Power(str, n - 1))) else Const(0)
          }
          case Times(aexp) => { diff_time(aexp, x) }
          case Sum(aexp) => { diff_sum(aexp, x) }
        }
    }
  }
  def diff_time(((expl, x))) = {
    expl match {
      case Nil() => { Const(1) }
      case Cons(hd, Nil()) => { diff(hd, x) }
      case Cons(hd, tl) => {
        Sum(List(Times(diff(hd, x) :: tl), Times(List(hd, diff(Times(tl), x)))))
      }
    }
  }
  def diff_sum(((expl, x))) = {
    expl match {
      case Nil() => { Const(0) }
      case Cons(hd, Nil()) => { diff(hd, x) }
      case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
    }
  }
  
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
}