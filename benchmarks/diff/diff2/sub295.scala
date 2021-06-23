import stainless.collection._
import stainless.io.StdOut._
import stainless.lang._
import stainless.math._
import stainless.math.BitVectors._

object benchmarks_diff_diff2_sub295 {
  sealed abstract class Aexp {}
  case class Const(param0: Int63) extends Aexp {}
  case class Var(param0: String) extends Aexp {}
  case class Power(param0: String,  param1: Int63) extends Aexp {}
  case class Times(param0: List[Aexp]) extends Aexp {}
  case class Sum(param0: List[Aexp]) extends Aexp {}
    
  def isExist(exp: Aexp, x: String): Boolean = {
    exp match {
      case Const(n) => { false }
      case Var(v) => { if (v == x) true else false }
      case Power(v, n) => { if (v == x) true else false }
      case Times(t) => {
        val _5 = {
          def reTime(lst) = {
            lst match {
              case Nil() => { false }
              case Cons(hd, tl) => { isExist(hd, x) || reTime(tl) }
            }
          }
          reTime(t)
        }
      }
      case Sum(s) => {
        val _2 = {
          def reSum(lst) = {
            lst match {
              case Nil() => { false }
              case Cons(hd, tl) => { isExist(hd, x) || reSum(tl) }
            }
          }
          reSum(s)
        }
      }
    }
  }
    
  def diff: (Aexp, String) => Aexp = {
    case (exp, x) =>
      {
        exp match {
          case Const(n) => { Const(0) }
          case Var(v) => { if (v == x) Const(1) else Var(v) }
          case Power(v, n) => {
            if (v == x) Times(List(Const(n), Power(v, n - 1))) else Power(v, n)
          }
          case Times(t) => {
            
              if (
                not(isExist(Times(t), x))
              ) {
                Const(0) 
              } else {
                val _13 = {
                  def reTime(lst) = {
                    lst match {
                      case Nil() => { Nil() }
                      case Cons(hd, tl) => {
                        hd match {
                          case Const(n) => { hd :: reTime(tl) }
                          case _ => { diff(hd, x) :: reTime(tl) }
                        }
                      }
                    }
                  }
                  Times(reTime(t))
                }
              }
          }
          case Sum(s) => {
            
              if (
                not(isExist(Sum(s), x))
              ) {
                Const(0) 
              } else {
                val _9 = {
                  def reSum(lst) = {
                    lst match {
                      case Nil() => { Nil() }
                      case Cons(hd, tl) => { diff(hd, x) :: reSum(tl) }
                    }
                  }
                  Sum(reSum(s))
                }
              }
          }
        }
    }
  }
        
  diff(Sum(List(Power("x", 2), Times(List(Const(2), Var("x"))), Const(1))), "y")
  diff(Sum(List(Times(List(Var("y"), Var("x"))), Const(2))), "y")
}