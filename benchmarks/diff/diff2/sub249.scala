import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub249 {
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
            
              if (
                str == x
              ) {
                
                  if (
                    n == 2
                  ) {
                    Times(List(Const(n), Var(x))) 
                  } else {
                    Times(List(Const(n), Power(str, n - 1)))
                  } 
              } else {
                Const(0)
              }
          }
          case Times(Cons(Const(n), Cons(Var(str), Nil()))) => {
            if (str == x) Const(n) else Const(0)
          }
          case Times(Cons(Const(n1), Cons(Power(str, n2), Nil()))) => {
            
              if (
                str == x
              ) {
                
                  if (
                    n2 == 2
                  ) {
                    Times(List(Const(n1 * n2), Var(x))) 
                  } else {
                    Times(List(Const(n1 * n2), Power(x, n2 - 1)))
                  } 
              } else {
                Const(0)
              }
          }
          case Times(lst) => { Times(lst) }
          case Sum(Cons(aexp, Nil())) => { diff(aexp, x) }
          case Sum(lst) => {
            lst match {
              case Cons(hd, tl) => { Sum(List(diff(hd, x), diff(Sum(tl), x))) }
              case Nil() => { Sum(Nil()) }
            }
          }
        }
    }
  }
  
  
  
  diff(Sum(List(Power("x", 2), Times(List(Const(3), Var("x"))), Const(4))), "x")
  diff(
    Sum(
      List(Times(List(Const(4), Power("x", 3))), Power("x", 2),
       Times(List(Const(2), Var("x"))), Const(4))),
    "x")
}