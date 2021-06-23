import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub274 {
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
          case Var(a) => { if (a == x) Const(1) else Var(a) }
          case Power(a, b) => {
            if (a == x) Times(List(Const(b), Power(a, b - 1))) else Power(a, b)
          }
          case Times(l) => { diff_times(l, x) }
          case Sum(l) => { Sum(diff_sum(l, x)) }
        }
    }
  }
  def diff_times(((l, x))) = {
    l match {
      case Nil() => { Const(0) }
      case Cons(h, t) => {
        Sum(
          List(Times(diff(h, x) :: t)) ++
          List(Times(List(h, diff_times(t, x)))))
      }
    }
  }
  def diff_sum(((l, x))) = {
    l match {
      case Nil() => { Nil() }
      case Cons(h, t) => { diff(h, x) :: diff_sum(t, x) }
    }
  }
      
  /*diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "x");;*/
}