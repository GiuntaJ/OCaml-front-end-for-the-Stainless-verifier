import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub91 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        exp match {
          case Const(i) => { Const(0) }
          case Var(s) => { if (s == var0) Const(1) else Const(0) }
          case Power(s, i) => {
            if (s == var0) Times(List(Const(i), Power(s, i - 1))) else Const(0)
          }
          case Times(lst) => {
            lst match {
              case Nil() => { Const(1) }
              case Cons(hd, tl) => {
                Times(List(diff_times_helper(hd, var0), diff(Times(tl), var0)))
              }
            }
          }
          case Sum(lst) => {
            lst match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => {
                Sum(List(diff(hd, var0), diff(Sum(tl), var0)))
              }
            }
          }
        }
    }
  }
  def diff_times_helper(((exp, var0))) = {
    exp match {
      case Const(i) => { Const(i) }
      case Var(s) => { if (s == var0) Const(1) else Var(s) }
      case Power(s, i) => {
        if (s == var0) Times(List(Const(i), Power(s, i - 1))) else Power(s, i)
      }
      case _ => { diff(exp, var0) }
    }
  }
}