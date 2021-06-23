import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub264 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def timehelp(lst) = {
    lst match {
      case Nil() => { Times(Nil()) }
      case Cons(hd, tl) => { Times(List(hd, Times(tl))) }
    }
  }
    
  def sumhelp(lst) = {
    lst match {
      case Nil() => { Sum(Nil()) }
      case Cons(hd, tl) => { Sum(List(hd, Sum(tl))) }
    }
  }
    
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        exp match {
          case Const(a) => { Const(0) }
          case Var(x) => { Const(1) }
          case Power(x, a) => { Times(List(Const(a), Power(x, a - 1))) }
          case Times(Cons(a, Cons(b, Nil()))) => {
            Sum(List(Times(List(diff(a, x), b)), Times(List(a, diff(b, x)))))
          }
          case Times(Cons(a, Nil())) => { diff(a, x) }
          case Times(Nil()) => { Const(0) }
          case Times(lst) => { diff(timehelp(lst), x) }
          case Sum(Cons(a, Cons(b, Nil()))) => {
            Sum(List(diff(a, x), diff(b, x)))
          }
          case Sum(Cons(a, Nil())) => { diff(a, x) }
          case Sum(Nil()) => { Const(0) }
          case Sum(lst) => { diff(sumhelp(lst), x) }
        }
    }
  }
      
  diff(Sum(List(Times(List(Const(2), Var("x"))), Const(2))), "x")
  
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "x")
}
