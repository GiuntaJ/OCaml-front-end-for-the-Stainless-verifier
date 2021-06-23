import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub247 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
  
  def search: (List[A], A) => Boolean = {
    case (lst, a) =>
      {
        lst match {
          case Nil() => { true }
          case Cons(hd, tl) => { if (hd == a) false else search(tl, a) }
        }
    }
  }
  
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(variable) => { if (variable == x) Const(1) else Var(variable)
          }
          case Power(str, n) => {
            
              if (
                n == 0
              ) {
                Const(0) 
              } else if (
                n == 1
              ) {
                Const(1) 
              } else {
                Times(List(Const(n), Power(str, n - 1)))
              }
          }
          case Times(lst) => {
            val _5 = {
              def f(l1, l2, a) = {
                l1 match {
                  case Nil() => {
                    
                      if (
                        a == 1
                      ) {
                        Times(Const(a) :: l2) 
                      } else {
                        Times(Const(a) :: Power(x, a - 1) :: l2)
                      }
                  }
                  case Cons(hd, tl) => {
                    hd match {
                      case Var(x) => { f(tl, diff(hd, x) :: l2, a + 1) }
                      case Const(n) => { f(tl, hd :: l2, a) }
                      case _ => { f(tl, diff(hd, x) :: l2, a) }
                    }
                  }
                }
              }
              f(lst, Nil(), 0)
            }
          }
          case Sum(lst) => {
            val _2 = {
              def f(l1, l2) = {
                l1 match {
                  case Nil() => { Sum(l2) }
                  case Cons(hd, tl) => { f(tl, diff(hd, x) :: l2) }
                }
              }
              f(lst, Nil())
            }
          }
        }
    }
  }
}