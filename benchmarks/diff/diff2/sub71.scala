import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub71 {
  
    sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {} 
  
    def diff: (Aexp, String) => Aexp = {
    case (exp, var0) =>
      {
        (exp, var0) match {
          case (Const(n), v) => { Const(0) }
          case (Var(x), v) => { if (x == v) Const(1) else Const(0) }
          case (Power(v1, n), v) => {
            if (v1 == v) Times(List(Const(n), Power(v1, n - 1))) else Const(0)
          }
          case (Times(l), v) => {
            l match {
              case Cons(Const(n), tl) => { Times(Const(n * iter(v, l)) :: tl) }
              case _ => { Const(0) }
            }
          }
          case (Sum(l), v) => {
            l match {
              case Nil() => { Const(0) }
              case Cons(hd, tl) => { Sum(List(diff(hd, v), diff(Sum(tl), v))) }
            }
          }
        }
    }
  }
  def count(x, e) = {
    e match {
      case Const(n) => { 0 }
      case Var(v) => { if (x == v) 1 else 0 }
      case Power(v, n) => { if (v == x) n else 0 }
      case Times(l) => { iter(x, l) }
      case Sum(l) => { 0 }
    }
  }
  def iter(x, l) = {
    l match {
      case Nil() => { 0 }
      case Cons(hd, tl) => { count(x, hd) + iter(x, tl) }
    }
  }
}