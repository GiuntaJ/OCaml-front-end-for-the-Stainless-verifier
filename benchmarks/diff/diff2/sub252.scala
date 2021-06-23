import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub252 {
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
          case Const(_) => { Const(0) }
          case Var(a) => { if (a == x) Const(1) else Const(0) }
          case Power(a, n) => {
            if (a == x) Times(List(Const(n), Power(x, n - 1))) else Const(0)
          }
          case Sum(slst) => { Sum(diff_sum(slst, x)) }
          case Times(tlst) => { Sum(diff_times(Nil(), tlst, x)) }
        }
    }
  }
  def diff_sum: (List[Aexp], String) => List[Aexp] = {
    case (slst, x) =>
      {
        slst match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => { diff(hd, x) :: diff_sum(tl, x) }
        }
    }
  }
  def diff_times: (List[Aexp], List[Aexp], String) => List[Aexp] = {
    case (left, right, x) =>
      {
        right match {
          case Nil() => { Nil() }
          case Cons(hd, tl) => {
            Times((left ++ List(diff(hd, x))) ++ tl) ::
            diff_times(left ++ List(hd), tl, x)
          }
        }
    }
  }
}