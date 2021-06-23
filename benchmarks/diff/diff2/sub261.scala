import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub261 {
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
          case Const(p) => { Const(0) }
          case Var(p) => { if (p == x) Const(1) else Const(0) }
          case Power(p, n) => {
            if (p == x) Times(List(Const(n), Power(p, n - 1))) else Const(0)
          }
          case Times(p) => { jun1(p, x) }
          case Sum(p) => { Sum(jun(p, x)) }
        }
    }
  }
  def jun1(p, x) = {
    p match {
      case Nil() => { Const(0) }
      case Cons(hd, tl) => {
        Sum(
          List(Times(diff(hd, x) :: tl),
           Times(List(hd) ++ List(diff(Times(tl), x)))))
      }
    }
  }
  def jun: (List[Aexp], String) => List[Aexp] = {
    case (q, x) =>
      {
        q match {
          case Cons(hd, tl) => {
            val _2 = {
              val a = hd
              val b = tl
              b match {
                case Nil() => { List(diff(a, x)) }
                case Cons(hd, tl) => {
                  List(diff(a, x)) ++ List(diff(hd, x)) ++ jun(tl, x)
                }
              }
            }
          }
        }
    }
  }
              
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
}