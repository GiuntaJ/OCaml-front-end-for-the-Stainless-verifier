import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub266 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        (exp, x) match {
          case (Const(a), x) => { Const(0) }
          case (Var(a), x) => { if (a == x) Const(1) else Var(a) }
          case (Power(a, b), x) => {
            if (a == x) Times(List(Const(b), Power(a, b - 1))) else Power(a, b)
          }
          case (Times(li), x) => { Sum(diff_time(li, x)) }
          case (Sum(li), x) => { Sum(diff_sum(li, x)) }
        }
    }
  }
  def diff_time: (List[Aexp], String) => List[Aexp] = {
    case (li, x) =>
      {
        li match {
          case Nil() => { Nil() }
          case Cons(h, t) => {
            
              if (
                t != Nil()
              ) {
                List(Times(List(diff(h, x)) ++ t)) ++
                List(Times(List(h) ++ diff_time(t, x))) 
              } else {
                List(Times(List(diff(h, x)) ++ t))
              }
          }
        }
    }
  }
  def diff_sum: (List[Aexp], String) => List[Aexp] = {
    case (li, x) =>
      {
        li match {
          case Nil() => { Nil() }
          case Cons(h, t) => { List(diff(h, x)) ++ diff_sum(t, x) }
        }
    }
  }
  
  /*diff (Times[Const 2; Var "x" ], "x" );;*/
  /*diff (Sum[Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "x");;*/
  /*diff ( Power ("x",-3) , "x");;*/
}