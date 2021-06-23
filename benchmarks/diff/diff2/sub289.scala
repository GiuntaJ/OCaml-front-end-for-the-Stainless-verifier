import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub289 {
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
          case Const(c) => { Const(0) }
          case Power(str, 0) => { Const(1) }
          case Power(str, 1) => { Var(str) }
          case Power(str, n) => { Times(List(Const(n), Power(str, n - 1))) }
          case Times(Cons(Const(n), Cons(Var(str), Nil()))) => { Const(n) }
          case Times(Cons(Const(n), Cons(ex, Nil()))) => {
            Times(List(Const(n), diff(ex, x)))
          }
          case Times(Cons(ex, Cons(Var(str), Nil()))) => {
            Times(List(diff(ex, x), Var(str)))
          }
          case Times(Cons(ex1, Cons(ex2, Nil()))) => {
            Times(List(diff(ex1, x), diff(ex2, x)))
          }
          case Sum(Cons(Const(n), Nil())) => { Const(n) }
          case Sum(Cons(hd, Cons(tl, Nil()))) => {
            Sum(List(diff(hd, x), diff(tl, x)))
          }
          case Sum(Cons(h1, Cons(h2, Cons(tl, Nil())))) => {
            Sum(List(diff(h1, x), diff(h2, x), diff(tl, x)))
          }
        }
    }
  }
  
  /*
  For example, 
  (square of x) + 2x + 1 is represented by
  Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1]
  and differentiating it (w.r.t. “x”) gives 2x + 2, which can be represented by
  Sum [Times [Const 2; Var "x"]; Const 2]
  
  */
}