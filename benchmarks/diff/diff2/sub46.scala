import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub46 {
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
          case Const(c) => { Const(0) }
          case Var(a) => { if (a == x) Const(1) else Const(0) }
          case Power(a, n) => { Times(List(Const(n), Power(a, n - 1))) }
          case Times(l) => { Sum(help_times(l, l, 0, x)) }
          case Sum(l) => { Sum(help_sum(l, x)) }
        }
    }
  }
  def help_sum(((l, x))) = {
    l match {
      case Nil() => { Nil() }
      case Cons(h, t) => { diff(h, x) :: help_sum(t, x) }
    }
  }
  def help_times1(((l, n, x))) = {
    l match {
      case Nil() => { Nil() }
      case Cons(h, t) => {
        if (n == 0) help_times1(t, n - 1, x) else h :: help_times1(t, n - 1, x)
      }
    }
  }
  def help_times(((ll, l, n, x))) = {
    l match {
      case Nil() => { Nil() }
      case Cons(h, t) => {
        Times(diff(h, x) :: help_times1(ll, n, x)) ::
        help_times(ll, t, n + 1, x)
      }
    }
  }
}